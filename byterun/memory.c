/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: memory.c 9153 2008-12-03 18:09:09Z doligez $ */

#include <stdlib.h>
#include <string.h>
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"

extern uintnat llama_percent_free;                   /* major_gc.c */

/* Page table management */

#define Page(p) ((uintnat) (p) >> Page_log)
#define Page_mask ((uintnat) -1 << Page_log)

#ifdef ARCH_SIXTYFOUR

/* 64-bit implementation:
   The page table is represented sparsely as a hash table
   with linear probing */

struct page_table {
  mlsize_t size;                /* size == 1 << (wordsize - shift) */
  int shift;
  mlsize_t mask;                /* mask == size - 1 */
  mlsize_t occupancy;
  uintnat * entries;            /* [size]  */
};

static struct page_table llama_page_table;

/* Page table entries are the logical 'or' of
   - the key: address of a page (low Page_log bits = 0)
   - the data: a 8-bit integer */

#define Page_entry_matches(entry,addr) \
  ((((entry) ^ (addr)) & Page_mask) == 0)

/* Multiplicative Fibonacci hashing
   (Knuth, TAOCP vol 3, section 6.4, page 518).
   HASH_FACTOR is (sqrt(5) - 1) / 2 * 2^wordsize. */
#ifdef ARCH_SIXTYFOUR
#define HASH_FACTOR 11400714819323198486UL
#else
#define HASH_FACTOR 2654435769UL
#endif
#define Hash(v) (((v) * HASH_FACTOR) >> llama_page_table.shift)

int llama_page_table_lookup(void * addr)
{
  uintnat h, e;

  h = Hash(Page(addr));
  /* The first hit is almost always successful, so optimize for this case */
  e = llama_page_table.entries[h];
  if (Page_entry_matches(e, (uintnat)addr)) return e & 0xFF;
  while(1) {
    if (e == 0) return 0;
    h = (h + 1) & llama_page_table.mask;
    e = llama_page_table.entries[h];
    if (Page_entry_matches(e, (uintnat)addr)) return e & 0xFF;
  }
}

int llama_page_table_initialize(mlsize_t bytesize)
{
  uintnat pagesize = Page(bytesize);

  llama_page_table.size = 1;
  llama_page_table.shift = 8 * sizeof(uintnat);
  /* Aim for initial load factor between 1/4 and 1/2 */
  while (llama_page_table.size < 2 * pagesize) {
    llama_page_table.size <<= 1;
    llama_page_table.shift -= 1;
  }
  llama_page_table.mask = llama_page_table.size - 1;
  llama_page_table.occupancy = 0;
  llama_page_table.entries = calloc(llama_page_table.size, sizeof(uintnat));
  if (llama_page_table.entries == NULL)
    return -1;
  else
    return 0;
}

static int llama_page_table_resize(void)
{
  struct page_table old = llama_page_table;
  uintnat * new_entries;
  uintnat i, h;

  llama_gc_message (0x08, "Growing page table to %lu entries\n",
                   llama_page_table.size);

  new_entries = calloc(2 * old.size, sizeof(uintnat));
  if (new_entries == NULL) {
    llama_gc_message (0x08, "No room for growing page table\n", 0);
    return -1;
  }

  llama_page_table.size = 2 * old.size;
  llama_page_table.shift = old.shift - 1;
  llama_page_table.mask = llama_page_table.size - 1;
  llama_page_table.occupancy = old.occupancy;
  llama_page_table.entries = new_entries;

  for (i = 0; i < old.size; i++) {
    uintnat e = old.entries[i];
    if (e == 0) continue;
    h = Hash(Page(e));
    while (llama_page_table.entries[h] != 0)
      h = (h + 1) & llama_page_table.mask;
    llama_page_table.entries[h] = e;
  }

  free(old.entries);
  return 0;
}

static int llama_page_table_modify(uintnat page, int toclear, int toset)
{
  uintnat h;

  Assert ((page & ~Page_mask) == 0);

  /* Resize to keep load factor below 1/2 */
  if (llama_page_table.occupancy * 2 >= llama_page_table.size) {
    if (llama_page_table_resize() != 0) return -1;
  }
  h = Hash(Page(page));
  while (1) {
    if (llama_page_table.entries[h] == 0) {
      llama_page_table.entries[h] = page | toset;
      llama_page_table.occupancy++;
      break;
    }
    if (Page_entry_matches(llama_page_table.entries[h], page)) {
      llama_page_table.entries[h] =
        (llama_page_table.entries[h] & ~toclear) | toset;
      break;
    }
    h = (h + 1) & llama_page_table.mask;
  }
  return 0;
}

#else

/* 32-bit implementation:
   The page table is represented as a 2-level array of unsigned char */

CAMLexport unsigned char * llama_page_table[Pagetable1_size];
static unsigned char llama_page_table_empty[Pagetable2_size] = { 0, };

int llama_page_table_initialize(mlsize_t bytesize)
{
  int i;
  for (i = 0; i < Pagetable1_size; i++)
    llama_page_table[i] = llama_page_table_empty;
  return 0;
}

static int llama_page_table_modify(uintnat page, int toclear, int toset)
{
  uintnat i = Pagetable_index1(page);
  uintnat j = Pagetable_index2(page);

  if (llama_page_table[i] == llama_page_table_empty) {
    unsigned char * new_tbl = calloc(Pagetable2_size, 1);
    if (new_tbl == 0) return -1;
    llama_page_table[i] = new_tbl;
  }
  llama_page_table[i][j] = (llama_page_table[i][j] & ~toclear) | toset;
  return 0;
}

#endif

int llama_page_table_add(int kind, void * start, void * end)
{
  uintnat pstart = (uintnat) start & Page_mask;
  uintnat pend = ((uintnat) end - 1) & Page_mask;
  uintnat p;

  for (p = pstart; p <= pend; p += Page_size)
    if (llama_page_table_modify(p, 0, kind) != 0) return -1;
  return 0;
}

int llama_page_table_remove(int kind, void * start, void * end)
{
  uintnat pstart = (uintnat) start & Page_mask;
  uintnat pend = ((uintnat) end - 1) & Page_mask;
  uintnat p;

  for (p = pstart; p <= pend; p += Page_size)
    if (llama_page_table_modify(p, kind, 0) != 0) return -1;
  return 0;
}

/* Allocate a block of the requested size, to be passed to
   [llama_add_to_heap] later.
   [request] must be a multiple of [Page_size].
   [llama_alloc_for_heap] returns NULL if the request cannot be satisfied.
   The returned pointer is a hp, but the header must be initialized by
   the caller.
*/
char *llama_alloc_for_heap (asize_t request)
{
  char *mem;
  void *block;
                                              Assert (request % Page_size == 0);
  mem = llama_aligned_malloc (request + sizeof (heap_chunk_head),
                             sizeof (heap_chunk_head), &block);
  if (mem == NULL) return NULL;
  mem += sizeof (heap_chunk_head);
  Chunk_size (mem) = request;
  Chunk_block (mem) = block;
  return mem;
}

/* Use this function to free a block allocated with [llama_alloc_for_heap]
   if you don't add it with [llama_add_to_heap].
*/
void llama_free_for_heap (char *mem)
{
  free (Chunk_block (mem));
}

/* Take a chunk of memory as argument, which must be the result of a
   call to [llama_alloc_for_heap], and insert it into the heap chaining.
   The contents of the chunk must be a sequence of valid blocks and
   fragments: no space between blocks and no trailing garbage.  If
   some blocks are blue, they must be added to the free list by the
   caller.  All other blocks must have the color [llama_allocation_color(m)].
   The caller must update [llama_allocated_words] if applicable.
   Return value: 0 if no error; -1 in case of error.
*/
int llama_add_to_heap (char *m)
{
                                     Assert (Chunk_size (m) % Page_size == 0);
#ifdef DEBUG
  /* Should check the contents of the block. */
#endif /* debug */

  llama_gc_message (0x04, "Growing heap to %luk bytes\n",
                   (llama_stat_heap_size + Chunk_size (m)) / 1024);

  /* Register block in page table */
  if (llama_page_table_add(In_heap, m, m + Chunk_size(m)) != 0)
    return -1;

  /* Chain this heap chunk. */
  {
    char **last = &llama_heap_start;
    char *cur = *last;

    while (cur != NULL && cur < m){
      last = &(Chunk_next (cur));
      cur = *last;
    }
    Chunk_next (m) = cur;
    *last = m;

    ++ llama_stat_heap_chunks;
  }

  llama_stat_heap_size += Chunk_size (m);
  if (llama_stat_heap_size > llama_stat_top_heap_size){
    llama_stat_top_heap_size = llama_stat_heap_size;
  }
  return 0;
}

/* Allocate more memory from malloc for the heap.
   Return a blue block of at least the requested size.
   The blue block is chained to a sequence of blue blocks (through their
   field 0); the last block of the chain is pointed by field 1 of the
   first.  There may be a fragment after the last block.
   The caller must insert the blocks into the free list.
   The request must be less than or equal to Max_wosize.
   Return NULL when out of memory.
*/
static char *expand_heap (mlsize_t request)
{
  char *mem, *hp, *prev;
  asize_t over_request, malloc_request, remain;

  Assert (request <= Max_wosize);
  over_request = request + request / 100 * llama_percent_free;
  malloc_request = llama_round_heap_chunk_size (Bhsize_wosize (over_request));
  mem = llama_alloc_for_heap (malloc_request);
  if (mem == NULL){
    llama_gc_message (0x04, "No room for growing heap\n", 0);
    return NULL;
  }
  remain = malloc_request;
  prev = hp = mem;
  /* XXX find a way to do this with a call to llama_make_free_blocks */
  while (Wosize_bhsize (remain) > Max_wosize){
    Hd_hp (hp) = Make_header (Max_wosize, 0, Caml_blue);
#ifdef DEBUG
    llama_set_fields (Bp_hp (hp), 0, Debug_free_major);
#endif
    hp += Bhsize_wosize (Max_wosize);
    remain -= Bhsize_wosize (Max_wosize);
    Field (Op_hp (mem), 1) = Field (Op_hp (prev), 0) = (value) Op_hp (hp);
    prev = hp;
  }
  if (remain > 1){
    Hd_hp (hp) = Make_header (Wosize_bhsize (remain), 0, Caml_blue);
#ifdef DEBUG
    llama_set_fields (Bp_hp (hp), 0, Debug_free_major);
#endif
    Field (Op_hp (mem), 1) = Field (Op_hp (prev), 0) = (value) Op_hp (hp);
    Field (Op_hp (hp), 0) = (value) NULL;
  }else{
    Field (Op_hp (prev), 0) = (value) NULL;
    if (remain == 1) Hd_hp (hp) = Make_header (0, 0, Caml_white);
  }
  Assert (Wosize_hp (mem) >= request);
  if (llama_add_to_heap (mem) != 0){
    llama_free_for_heap (mem);
    return NULL;
  }
  return Bp_hp (mem);
}

/* Remove the heap chunk [chunk] from the heap and give the memory back
   to [free].
*/
void llama_shrink_heap (char *chunk)
{
  char **cp;

  /* Never deallocate the first block, because llama_heap_start is both the
     first block and the base address for page numbers, and we don't
     want to shift the page table, it's too messy (see above).
     It will never happen anyway, because of the way compaction works.
     (see compact.c)
  */
  if (chunk == llama_heap_start) return;

  llama_stat_heap_size -= Chunk_size (chunk);
  llama_gc_message (0x04, "Shrinking heap to %luk bytes\n",
                   (unsigned long) llama_stat_heap_size / 1024);

#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wsize_bsize (Chunk_size (chunk)); i++){
      ((value *) chunk) [i] = Debug_free_shrink;
    }
  }
#endif

  -- llama_stat_heap_chunks;

  /* Remove [chunk] from the list of chunks. */
  cp = &llama_heap_start;
  while (*cp != chunk) cp = &(Chunk_next (*cp));
  *cp = Chunk_next (chunk);

  /* Remove the pages of [chunk] from the page table. */
  llama_page_table_remove(In_heap, chunk, chunk + Chunk_size (chunk));

  /* Free the [malloc] block that contains [chunk]. */
  llama_free_for_heap (chunk);
}

color_t llama_allocation_color (void *hp)
{
  if (llama_gc_phase == Phase_mark
      || (llama_gc_phase == Phase_sweep && (addr)hp >= (addr)llama_gc_sweep_hp)){
    return Caml_black;
  }else{
    Assert (llama_gc_phase == Phase_idle
            || (llama_gc_phase == Phase_sweep
                && (addr)hp < (addr)llama_gc_sweep_hp));
    return Caml_white;
  }
}

CAMLexport value llama_alloc_shr (mlsize_t wosize, tag_t tag)
{
  char *hp, *new_block;

  if (wosize > Max_wosize) llama_raise_out_of_memory ();
  hp = llama_fl_allocate (wosize);
  if (hp == NULL){
    new_block = expand_heap (wosize);
    if (new_block == NULL) {
      if (llama_in_minor_collection)
        llama_fatal_error ("Fatal error: out of memory.\n");
      else
        llama_raise_out_of_memory ();
    }
    llama_fl_add_blocks (new_block);
    hp = llama_fl_allocate (wosize);
  }

  Assert (Is_in_heap (Val_hp (hp)));

  /* Inline expansion of llama_allocation_color. */
  if (llama_gc_phase == Phase_mark
      || (llama_gc_phase == Phase_sweep && (addr)hp >= (addr)llama_gc_sweep_hp)){
    Hd_hp (hp) = Make_header (wosize, tag, Caml_black);
  }else{
    Assert (llama_gc_phase == Phase_idle
            || (llama_gc_phase == Phase_sweep
                && (addr)hp < (addr)llama_gc_sweep_hp));
    Hd_hp (hp) = Make_header (wosize, tag, Caml_white);
  }
  Assert (Hd_hp (hp) == Make_header (wosize, tag, llama_allocation_color (hp)));
  llama_allocated_words += Whsize_wosize (wosize);
  if (llama_allocated_words > Wsize_bsize (llama_minor_heap_size)){
    llama_urge_major_slice ();
  }
#ifdef DEBUG
  {
    uintnat i;
    for (i = 0; i < wosize; i++){
      Field (Val_hp (hp), i) = Debug_uninit_major;
    }
  }
#endif
  return Val_hp (hp);
}

/* Dependent memory is all memory blocks allocated out of the heap
   that depend on the GC (and finalizers) for deallocation.
   For the GC to take dependent memory into account when computing
   its automatic speed setting,
   you must call [llama_alloc_dependent_memory] when you alloate some
   dependent memory, and [llama_free_dependent_memory] when you
   free it.  In both cases, you pass as argument the size (in bytes)
   of the block being allocated or freed.
*/
CAMLexport void llama_alloc_dependent_memory (mlsize_t nbytes)
{
  llama_dependent_size += nbytes / sizeof (value);
  llama_dependent_allocated += nbytes / sizeof (value);
}

CAMLexport void llama_free_dependent_memory (mlsize_t nbytes)
{
  if (llama_dependent_size < nbytes / sizeof (value)){
    llama_dependent_size = 0;
  }else{
    llama_dependent_size -= nbytes / sizeof (value);
  }
}

/* Use this function to tell the major GC to speed up when you use
   finalized blocks to automatically deallocate resources (other
   than memory). The GC will do at least one cycle every [max]
   allocated resources; [res] is the number of resources allocated
   this time.
   Note that only [res/max] is relevant.  The units (and kind of
   resource) can change between calls to [llama_adjust_gc_speed].
*/
CAMLexport void llama_adjust_gc_speed (mlsize_t res, mlsize_t max)
{
  if (max == 0) max = 1;
  if (res > max) res = max;
  llama_extra_heap_resources += (double) res / (double) max;
  if (llama_extra_heap_resources > 1.0){
    llama_extra_heap_resources = 1.0;
    llama_urge_major_slice ();
  }
  if (llama_extra_heap_resources
           > (double) Wsize_bsize (llama_minor_heap_size) / 2.0
             / (double) Wsize_bsize (llama_stat_heap_size)) {
    llama_urge_major_slice ();
  }
}

/* You must use [llama_initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [llama_initialize] never calls the GC, so you may call it while an block is
   unfinished (i.e. just after a call to [llama_alloc_shr].) */
void llama_initialize (value *fp, value val)
{
  *fp = val;
  if (Is_block (val) && Is_young (val) && Is_in_heap (fp)){
    if (llama_ref_table.ptr >= llama_ref_table.limit){
      llama_realloc_ref_table (&llama_ref_table);
    }
    *llama_ref_table.ptr++ = fp;
  }
}

/* You must use [llama_modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [llama_modify] never calls the GC. */
void llama_modify (value *fp, value val)
{
  Modify (fp, val);
}

CAMLexport void * llama_stat_alloc (asize_t sz)
{
  void * result = malloc (sz);

  /* malloc() may return NULL if size is 0 */
  if (result == NULL && sz != 0) llama_raise_out_of_memory ();
#ifdef DEBUG
  memset (result, Debug_uninit_stat, sz);
#endif
  return result;
}

CAMLexport void llama_stat_free (void * blk)
{
  free (blk);
}

CAMLexport void * llama_stat_resize (void * blk, asize_t sz)
{
  void * result = realloc (blk, sz);

  if (result == NULL) llama_raise_out_of_memory ();
  return result;
}
