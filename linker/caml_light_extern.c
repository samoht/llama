/* Caml Light marshalling */

#include <string.h>
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/fail.h"

#define Base_magic_number 0x8495A6B9
#define Big_endian_32_magic_number Base_magic_number
#define Little_endian_32_magic_number (Base_magic_number + 1)
#define Big_endian_64_magic_number (Base_magic_number + 2)
#define Little_endian_64_magic_number (Base_magic_number + 3)
#ifdef ARCH_SIXTYFOUR
# ifdef ARCH_BIG_ENDIAN
#  define Extern_magic_number Big_endian_64_magic_number
# else
#  define Extern_magic_number Little_endian_64_magic_number
# endif
#else
# ifdef ARCH_BIG_ENDIAN
#  define Extern_magic_number Big_endian_32_magic_number
# else
#  define Extern_magic_number Little_endian_32_magic_number
# endif
#endif

#ifndef INITIAL_EXTERN_SIZE
#define INITIAL_EXTERN_SIZE 4096
#endif
#ifndef INITIAL_EXTERN_TABLE_SIZE
#define INITIAL_EXTERN_TABLE_SIZE 2039
#endif

typedef unsigned long byteoffset_t;

struct extern_obj {
  value obj;
  byteoffset_t ofs;
};

struct extern_obj * extern_table;
unsigned long extern_table_size, extern_table_used;

#ifdef ARCH_SIXTYFOUR
#define Hash(v) (((unsigned long) ((v) >> 3)) % extern_table_size)
#else
#define Hash(v) (((unsigned long) ((v) >> 2)) % extern_table_size)
#endif

static void alloc_extern_table() {
  asize_t i;

  extern_table = (struct extern_obj *)
    caml_stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++)
    extern_table[i].obj = 0;
}

static void resize_extern_table() {
  asize_t oldsize;
  struct extern_obj * oldtable;
  asize_t i, h;

  oldsize = extern_table_size;
  oldtable = extern_table;
  extern_table_size = 2 * extern_table_size;
  alloc_extern_table();
  for (i = 0; i < oldsize; i++) {
    h = Hash(oldtable[i].obj);
    while (extern_table[h].obj != 0) {
      h++;
      if (h >= extern_table_size) h = 0;
    }
    extern_table[h].obj = oldtable[i].obj;
    extern_table[h].ofs = oldtable[i].ofs;
  }
  stat_free((char *) oldtable);
}

static byteoffset_t * extern_block;
static asize_t extern_size, extern_pos;

static void resize_result() {
  extern_size = 2 * extern_size;
  extern_block = (byteoffset_t *)
    stat_resize((char *) extern_block, extern_size * sizeof(byteoffset_t));
}

static byteoffset_t emit(value v) {
  mlsize_t size;
  asize_t h;
  byteoffset_t res;
  value * p;
  byteoffset_t * q;
  asize_t end_pos;

  if (Is_long(v)) return (byteoffset_t) v;
#if 0
  if (!Is_atom(v) && !Is_young(v) && !Is_in_heap(v)) {
    invalid_argument("output_value: abstract value");
  }
#endif
  size = Wosize_val(v);
  if (size == 0) return (Tag_val(v) << 2) + 2;
  if (2 * extern_table_used >= extern_table_size) resize_extern_table();
  h = Hash(v);
  while (extern_table[h].obj != 0) {
    if (extern_table[h].obj == v) return extern_table[h].ofs;
    h++;
    if (h >= extern_table_size) h = 0;
  }
  end_pos = extern_pos + 1 + size;
  while (end_pos >= extern_size) resize_result();
  extern_block[extern_pos++] = Hd_val(v);
  res = extern_pos * sizeof(byteoffset_t);
  extern_table[h].obj = v;
  extern_table[h].ofs = res;
  extern_table_used++;
  for (p = &Field(v, 0), q = &extern_block[extern_pos]; size > 0; size--) {
    *q++ = *p++;
  }
  extern_pos = end_pos;
  return res;
}

static byteoffset_t emit_all(value root) {
  asize_t read_pos;
  byteoffset_t res;
  header_t hd;
  mlsize_t sz;
  byteoffset_t ofs;

  read_pos = extern_pos;
  res = emit(root);
  while (read_pos < extern_pos) {
    hd = (header_t) extern_block[read_pos++];
    sz = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:
    case Double_tag:
      extern_block[read_pos-1]++;  // convert to Caml Light tag
      read_pos += sz;
      break;
    case Abstract_tag:
    case Double_array_tag:
    case Custom_tag:
      caml_invalid_argument("output_value: unsupported value");
      break;
    case Closure_tag:
      caml_invalid_argument("output_value: functional value");
      break;
    default:
      while (sz > 0) {
        ofs = emit((value) extern_block[read_pos]);
        extern_block[read_pos] = ofs;
        read_pos++;
        sz--;
      }
      break;
    }
  }
  return res;
}

static void aux_putch(char** channel, int ch) {
    *(*channel)++ = ch;
}

static void aux_putword(char** channel, byteoffset_t w) {
    aux_putch(channel, w >> 24);
    aux_putch(channel, w >> 16);
    aux_putch(channel, w >> 8);
    aux_putch(channel, w);
}

static void aux_putblock(char** channel, char* data, int len) {
    memcpy(*channel, data, len);
    *channel += len;
}

CAMLprim value caml_light_output_value_to_string(value v) {
  CAMLparam1(v);
  byteoffset_t res;
  extern_size = INITIAL_EXTERN_SIZE;
  extern_block =
    (byteoffset_t *) stat_alloc(extern_size * sizeof(unsigned long));
  extern_pos = 0;
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  extern_table_used = 0;
  res = emit_all(v);
  stat_free((char *) extern_table);
  CAMLlocal1(result);
  result = caml_alloc_string((extern_pos == 0) ? 12 :
                             8 + extern_pos * sizeof(unsigned long));
  char *chan = String_val(result);
  aux_putword(&chan, Extern_magic_number);
  aux_putword(&chan, extern_pos);
  if (extern_pos == 0)
    aux_putword(&chan, res);
  else
    aux_putblock(&chan, (char *) extern_block, extern_pos * sizeof(unsigned long));
  stat_free((char *) extern_block);
  CAMLreturn(result);
}
