#include <stdio.h>
#include "config.h"
#include "mlvalues.h"
#include "stacks.h"
#include "gc_ctrl.h"
#include "globals.h"
#include "prims.h"
#include "api.h"
#include "alloc.h"
extern value interprete(code_t prog);

#define NUM_GLOBALS 20   // cf globals.h

header_t first_atoms[256];
code_t start_code;
asize_t code_size;

static void init_atoms()
{
  int i;
  for(i = 0; i < 256; i++) first_atoms[i] = Make_header(0, i, White);
}

void zebra_init(char** argv) {
  int verbose_init = 0, percent_free_init = Percent_free_def;
  long minor_heap_init = Minor_heap_def, heap_chunk_init = Heap_chunk_def;
    init_gc (minor_heap_init, heap_chunk_init, percent_free_init,
             verbose_init);
    init_stacks();
    init_atoms();
    global_data = alloc_shr(NUM_GLOBALS, 0);
    int i;
    for (i = 0; i < NUM_GLOBALS; i++) {
        initialize(&Field(global_data, i), 0);
    }
    modify(&Field(global_data, GLOBAL_DATA), global_data);
    sys_init(argv);
}

size_t zebra_count_globals() {
    return Wosize_val(global_data);
}

zebra_value zebra_get_global(size_t i) {
    return Field(global_data, i);
}

void zebra_set_global(size_t i, zebra_value v) {
    modify(&Field(global_data, i), v);
}

void zebra_realloc_globals(size_t sz) {
  mlsize_t requested_size, actual_size, i;
  value new_global_data;

  requested_size = (mlsize_t)sz;
  actual_size = Wosize_val(global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    new_global_data = alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      initialize(&Field(new_global_data, i), Field(global_data, i));
    for (i = actual_size; i < requested_size; i++){
      Field (new_global_data, i) = Val_long (0);
    }
    modify(&Field(new_global_data, GLOBAL_DATA), new_global_data);
    global_data = new_global_data;
  }
}

char* zebra_stat_alloc(size_t n) {
    return(stat_alloc(n));
}

void zebra_stat_free(char* v) {
    stat_free(v);
}

char* zebra_stat_resize(char* v, size_t n) {
    return stat_resize(v, n);
}

value zebra_interpret(char* prog, size_t offset, size_t len) {
#if defined(CAML_BIG_ENDIAN) && !defined(CAML_ALIGNMENT)
    fixup_endianness(&Byte(prog, 0), (asize_t) len);
#endif
    return interprete(&Byte(prog, offset));
}

char** zebra_available_primitives() {
  return names_of_cprim;
}

long zebra_Long_val(value v) { return Long_val(v); }
double zebra_Double_val(value v) { return Double_val(v); }
char* zebra_String_val(value v) { return String_val(v); }

value zebra_Val_long(long l) { return Val_long(l); }
value zebra_copy_double(double d) { return copy_double(d); }
value zebra_copy_string(char* s) { return copy_string(s); }

int zebra_Is_block(value v) { return Is_block(v); }

unsigned int zebra_obj_tag(value arg) {
    return Tag_val(arg);
}

size_t zebra_Wosize_val(value v) { return Wosize_val(v); }
value zebra_Field(value v, size_t i) { return Field(v, i); }
void zebra_Store_field(value v, size_t i, value fv) { modify(&Field(v, i), fv); }

value zebra_obj_new_block(unsigned int tg, size_t sz) {
    value res;
    if (sz == 0) return Atom(tg);
    res = alloc_shr(sz, tg);
    int i;
    for (i = 0; i < sz; i++)
        initialize(&Field(res, i), Val_long(0));
    return res;
}

#ifdef DEBUG
#include "debugger.h"
void zebra_set_trace_flag(int b) {
    trace_flag = b;
}
#else
void zebra_set_trace_flag(int b) {
    fprintf(stderr, "set_trace_flag: not available");
}
#endif
