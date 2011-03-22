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

/* $Id: major_gc.c 9410 2009-11-04 12:25:47Z doligez $ */

#include <limits.h>

#include "compact.h"
#include "custom.h"
#include "config.h"
#include "fail.h"
#include "finalise.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "weak.h"

uintnat llama_percent_free;
uintnat llama_major_heap_increment;
CAMLexport char *llama_heap_start;
char *llama_gc_sweep_hp;
int llama_gc_phase;        /* always Phase_mark, Phase_sweep, or Phase_idle */
static value *gray_vals;
static value *gray_vals_cur, *gray_vals_end;
static asize_t gray_vals_size;
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
uintnat llama_allocated_words;
uintnat llama_dependent_size, llama_dependent_allocated;
double llama_extra_heap_resources;
uintnat llama_fl_size_at_phase_change = 0;

extern char *llama_fl_merge;  /* Defined in freelist.c. */

static char *markhp, *chunk, *limit;

int llama_gc_subphase;     /* Subphase_{main,weak1,weak2,final} */
static value *weak_prev;

#ifdef DEBUG
static unsigned long major_gc_counter = 0;
#endif

static void realloc_gray_vals (void)
{
  value *new;

  Assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < llama_stat_heap_size / 128){
    llama_gc_message (0x08, "Growing gray_vals to %"
                           ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                     (intnat) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      llama_gc_message (0x08, "No room for growing gray_vals\n", 0);
      gray_vals_cur = gray_vals;
      heap_is_pure = 0;
    }else{
      gray_vals = new;
      gray_vals_cur = gray_vals + gray_vals_size;
      gray_vals_size *= 2;
      gray_vals_end = gray_vals + gray_vals_size;
    }
  }else{
    gray_vals_cur = gray_vals + gray_vals_size / 2;
    heap_is_pure = 0;
  }
}

void llama_darken (value v, value *p /* not used */)
{
  if (Is_block (v) && Is_in_heap (v)) {
    header_t h = Hd_val (v);
    tag_t t = Tag_hd (h);
    if (t == Infix_tag){
      v -= Infix_offset_val(v);
      h = Hd_val (v);
      t = Tag_hd (h);
    }
    CAMLassert (!Is_blue_hd (h));
    if (Is_white_hd (h)){
      if (t < No_scan_tag){
        Hd_val (v) = Grayhd_hd (h);
        *gray_vals_cur++ = v;
        if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
      }else{
        Hd_val (v) = Blackhd_hd (h);
      }
    }
  }
}

static void start_cycle (void)
{
  Assert (llama_gc_phase == Phase_idle);
  Assert (gray_vals_cur == gray_vals);
  llama_gc_message (0x01, "Starting new major GC cycle\n", 0);
  llama_darken_all_roots();
  llama_gc_phase = Phase_mark;
  llama_gc_subphase = Subphase_main;
  markhp = NULL;
#ifdef DEBUG
  ++ major_gc_counter;
  llama_heap_check ();
#endif
}

static void mark_slice (intnat work)
{
  value *gray_vals_ptr;  /* Local copy of gray_vals_cur */
  value v, child;
  header_t hd;
  mlsize_t size, i;

  llama_gc_message (0x40, "Marking %ld words\n", work);
  llama_gc_message (0x40, "Subphase = %ld\n", llama_gc_subphase);
  gray_vals_ptr = gray_vals_cur;
  while (work > 0){
    if (gray_vals_ptr > gray_vals){
      v = *--gray_vals_ptr;
      hd = Hd_val(v);
      Assert (Is_gray_hd (hd));
      Hd_val (v) = Blackhd_hd (hd);
      size = Wosize_hd (hd);
      if (Tag_hd (hd) < No_scan_tag){
        for (i = 0; i < size; i++){
          child = Field (v, i);
          if (Is_block (child) && Is_in_heap (child)) {
            hd = Hd_val (child);
            if (Tag_hd (hd) == Forward_tag){
              value f = Forward_val (child);
              if (Is_block (f)
                  && (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
                      || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag)){
                /* Do not short-circuit the pointer. */
              }else{
                Field (v, i) = f;
              }
            }
            else if (Tag_hd(hd) == Infix_tag) {
              child -= Infix_offset_val(child);
              hd = Hd_val(child);
            }
            if (Is_white_hd (hd)){
              Hd_val (child) = Grayhd_hd (hd);
              *gray_vals_ptr++ = child;
              if (gray_vals_ptr >= gray_vals_end) {
                gray_vals_cur = gray_vals_ptr;
                realloc_gray_vals ();
                gray_vals_ptr = gray_vals_cur;
              }
            }
          }
        }
      }
      work -= Whsize_wosize(size);
    }else if (markhp != NULL){
      if (markhp == limit){
        chunk = Chunk_next (chunk);
        if (chunk == NULL){
          markhp = NULL;
        }else{
          markhp = chunk;
          limit = chunk + Chunk_size (chunk);
        }
      }else{
        if (Is_gray_val (Val_hp (markhp))){
          Assert (gray_vals_ptr == gray_vals);
          *gray_vals_ptr++ = Val_hp (markhp);
        }
        markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = llama_heap_start;
      markhp = chunk;
      limit = chunk + Chunk_size (chunk);
    }else{
      switch (llama_gc_subphase){
      case Subphase_main: {
        /* The main marking phase is over.  Start removing weak pointers to
           dead values. */
        llama_gc_subphase = Subphase_weak1;
        weak_prev = &llama_weak_list_head;
      }
        break;
      case Subphase_weak1: {
        value cur, curfield;
        mlsize_t sz, i;
        header_t hd;

        cur = *weak_prev;
        if (cur != (value) NULL){
          hd = Hd_val (cur);
          sz = Wosize_hd (hd);
          for (i = 1; i < sz; i++){
            curfield = Field (cur, i);
          weak_again:
            if (curfield != llama_weak_none
                && Is_block (curfield) && Is_in_heap (curfield)){
              if (Tag_val (curfield) == Forward_tag){
                value f = Forward_val (curfield);
                if (Is_block (f)) {
                  if (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
                      || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag){
                    /* Do not short-circuit the pointer. */
                  }else{
                    Field (cur, i) = curfield = f;
                    goto weak_again;
                  }
                }
              }
              if (Is_white_val (curfield)){
                Field (cur, i) = llama_weak_none;
              }
            }
          }
          weak_prev = &Field (cur, 0);
          work -= Whsize_hd (hd);
        }else{
          /* Subphase_weak1 is done.  Start removing dead weak arrays. */
          llama_gc_subphase = Subphase_weak2;
          weak_prev = &llama_weak_list_head;
        }
      }
        break;
      case Subphase_weak2: {
        value cur;
        header_t hd;

        cur = *weak_prev;
        if (cur != (value) NULL){
          hd = Hd_val (cur);
          if (Color_hd (hd) == Llama_white){
            /* The whole array is dead, remove it from the list. */
            *weak_prev = Field (cur, 0);
          }else{
            weak_prev = &Field (cur, 0);
          }
          work -= 1;
        }else{
          /* Subphase_weak2 is done.  Handle finalised values. */
          gray_vals_cur = gray_vals_ptr;
          llama_final_update ();
          gray_vals_ptr = gray_vals_cur;
          llama_gc_subphase = Subphase_final;
        }
      }
        break;
      case Subphase_final: {
        /* Initialise the sweep phase. */
        gray_vals_cur = gray_vals_ptr;
        llama_gc_sweep_hp = llama_heap_start;
        llama_fl_init_merge ();
        llama_gc_phase = Phase_sweep;
        chunk = llama_heap_start;
        llama_gc_sweep_hp = chunk;
        limit = chunk + Chunk_size (chunk);
        work = 0;
        llama_fl_size_at_phase_change = llama_fl_cur_size;
      }
        break;
      default: Assert (0);
      }
    }
  }
  gray_vals_cur = gray_vals_ptr;
}

static void sweep_slice (intnat work)
{
  char *hp;
  header_t hd;

  llama_gc_message (0x40, "Sweeping %ld words\n", work);
  while (work > 0){
    if (llama_gc_sweep_hp < limit){
      hp = llama_gc_sweep_hp;
      hd = Hd_hp (hp);
      work -= Whsize_hd (hd);
      llama_gc_sweep_hp += Bhsize_hd (hd);
      switch (Color_hd (hd)){
      case Llama_white:
        if (Tag_hd (hd) == Custom_tag){
          void (*final_fun)(value) = Custom_ops_val(Val_hp(hp))->finalize;
          if (final_fun != NULL) final_fun(Val_hp(hp));
        }
        llama_gc_sweep_hp = llama_fl_merge_block (Bp_hp (hp));
        break;
      case Llama_blue:
        /* Only the blocks of the free-list are blue.  See [freelist.c]. */
        llama_fl_merge = Bp_hp (hp);
        break;
      default:          /* gray or black */
        Assert (Color_hd (hd) == Llama_black);
        Hd_hp (hp) = Whitehd_hd (hd);
        break;
      }
      Assert (llama_gc_sweep_hp <= limit);
    }else{
      chunk = Chunk_next (chunk);
      if (chunk == NULL){
        /* Sweeping is done. */
        ++ llama_stat_major_collections;
        work = 0;
        llama_gc_phase = Phase_idle;
      }else{
        llama_gc_sweep_hp = chunk;
        limit = chunk + Chunk_size (chunk);
      }
    }
  }
}

/* The main entry point for the GC.  Called after each minor GC.
   [howmuch] is the amount of work to do, 0 to let the GC compute it.
   Return the computed amount of work to do.
 */
intnat llama_major_collection_slice (intnat howmuch)
{
  double p, dp;
  intnat computed_work;
  /*
     Free memory at the start of the GC cycle (garbage + free list) (assumed):
                 FM = llama_stat_heap_size * llama_percent_free
                      / (100 + llama_percent_free)

     Assuming steady state and enforcing a constant allocation rate, then
     FM is divided in 2/3 for garbage and 1/3 for free list.
                 G = 2 * FM / 3
     G is also the amount of memory that will be used during this cycle
     (still assuming steady state).

     Proportion of G consumed since the previous slice:
                 PH = llama_allocated_words / G
                    = llama_allocated_words * 3 * (100 + llama_percent_free)
                      / (2 * llama_stat_heap_size * llama_percent_free)
     Proportion of extra-heap resources consumed since the previous slice:
                 PE = llama_extra_heap_resources
     Proportion of total work to do in this slice:
                 P  = max (PH, PE)
     Amount of marking work for the GC cycle:
                 MW = llama_stat_heap_size * 100 / (100 + llama_percent_free)
     Amount of sweeping work for the GC cycle:
                 SW = llama_stat_heap_size

     In order to finish marking with a non-empty free list, we will
     use 40% of the time for marking, and 60% for sweeping.

     If TW is the total work for this cycle,
                 MW = 40/100 * TW
                 SW = 60/100 * TW

     Amount of work to do for this slice:
                 W  = P * TW

     Amount of marking work for a marking slice:
                 MS = P * MW / (40/100)
                 MS = P * llama_stat_heap_size * 250 / (100 + llama_percent_free)
     Amount of sweeping work for a sweeping slice:
                 SS = P * SW / (60/100)
                 SS = P * llama_stat_heap_size * 5 / 3

     This slice will either mark MS words or sweep SS words.
  */

  if (llama_gc_phase == Phase_idle) start_cycle ();

  p = (double) llama_allocated_words * 3.0 * (100 + llama_percent_free)
      / Wsize_bsize (llama_stat_heap_size) / llama_percent_free / 2.0;
  if (llama_dependent_size > 0){
    dp = (double) llama_dependent_allocated * (100 + llama_percent_free)
         / llama_dependent_size / llama_percent_free;
  }else{
    dp = 0.0;
  }
  if (p < dp) p = dp;
  if (p < llama_extra_heap_resources) p = llama_extra_heap_resources;

  llama_gc_message (0x40, "allocated_words = %"
                         ARCH_INTNAT_PRINTF_FORMAT "u\n",
                   llama_allocated_words);
  llama_gc_message (0x40, "extra_heap_resources = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (llama_extra_heap_resources * 1000000));
  llama_gc_message (0x40, "amount of work to do = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (p * 1000000));

  if (llama_gc_phase == Phase_mark){
    computed_work = (intnat) (p * Wsize_bsize (llama_stat_heap_size) * 250
                              / (100 + llama_percent_free));
  }else{
    computed_work = (intnat) (p * Wsize_bsize (llama_stat_heap_size) * 5 / 3);
  }
  llama_gc_message (0x40, "ordered work = %ld words\n", howmuch);
  llama_gc_message (0x40, "computed work = %ld words\n", computed_work);
  if (howmuch == 0) howmuch = computed_work;
  if (llama_gc_phase == Phase_mark){
    mark_slice (howmuch);
    llama_gc_message (0x02, "!", 0);
  }else{
    Assert (llama_gc_phase == Phase_sweep);
    sweep_slice (howmuch);
    llama_gc_message (0x02, "$", 0);
  }

  if (llama_gc_phase == Phase_idle) llama_compact_heap_maybe ();

  llama_stat_major_words += llama_allocated_words;
  llama_allocated_words = 0;
  llama_dependent_allocated = 0;
  llama_extra_heap_resources = 0.0;
  return computed_work;
}

/* The minor heap must be empty when this function is called;
   the minor heap is empty when this function returns.
*/
/* This does not call llama_compact_heap_maybe because the estimations of
   free and live memory are only valid for a cycle done incrementally.
   Besides, this function is called by llama_compact_heap_maybe.
*/
void llama_finish_major_cycle (void)
{
  if (llama_gc_phase == Phase_idle) start_cycle ();
  while (llama_gc_phase == Phase_mark) mark_slice (LONG_MAX);
  Assert (llama_gc_phase == Phase_sweep);
  while (llama_gc_phase == Phase_sweep) sweep_slice (LONG_MAX);
  Assert (llama_gc_phase == Phase_idle);
  llama_stat_major_words += llama_allocated_words;
  llama_allocated_words = 0;
}

/* Make sure the request is at least Heap_chunk_min and round it up
   to a multiple of the page size.
*/
static asize_t clip_heap_chunk_size (asize_t request)
{
  if (request < Bsize_wsize (Heap_chunk_min)){
    request = Bsize_wsize (Heap_chunk_min);
  }
  return ((request + Page_size - 1) >> Page_log) << Page_log;
}

/* Make sure the request is >= llama_major_heap_increment, then call
   clip_heap_chunk_size, then make sure the result is >= request.
*/
asize_t llama_round_heap_chunk_size (asize_t request)
{
  asize_t result = request;

  if (result < llama_major_heap_increment){
    result = llama_major_heap_increment;
  }
  result = clip_heap_chunk_size (result);

  if (result < request){
    llama_raise_out_of_memory ();
    return 0; /* not reached */
  }
  return result;
}

void llama_init_major_heap (asize_t heap_size)
{
  llama_stat_heap_size = clip_heap_chunk_size (heap_size);
  llama_stat_top_heap_size = llama_stat_heap_size;
  Assert (llama_stat_heap_size % Page_size == 0);
  llama_heap_start = (char *) llama_alloc_for_heap (llama_stat_heap_size);
  if (llama_heap_start == NULL)
    llama_fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  Chunk_next (llama_heap_start) = NULL;
  llama_stat_heap_chunks = 1;

  if (llama_page_table_add(In_heap, llama_heap_start,
                          llama_heap_start + llama_stat_heap_size) != 0) {
    llama_fatal_error ("Fatal error: not enough memory for the initial page table.\n");
  }

  llama_fl_init_merge ();
  llama_make_free_blocks ((value *) llama_heap_start,
                         Wsize_bsize (llama_stat_heap_size), 1);
  llama_gc_phase = Phase_idle;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  if (gray_vals == NULL)
    llama_fatal_error ("Fatal error: not enough memory for the gray cache.\n");
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  llama_allocated_words = 0;
  llama_extra_heap_resources = 0.0;
}
