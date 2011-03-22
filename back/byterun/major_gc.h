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

/* $Id: major_gc.h 8766 2008-01-11 11:55:36Z doligez $ */

#ifndef LLAMA_MAJOR_GC_H
#define LLAMA_MAJOR_GC_H


#include "freelist.h"
#include "misc.h"

typedef struct {
  void *block;           /* address of the malloced block this chunk live in */
  asize_t alloc;         /* in bytes, used for compaction */
  asize_t size;          /* in bytes */
  char *next;
} heap_chunk_head;

#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
#define Chunk_alloc(c) (((heap_chunk_head *) (c)) [-1]).alloc
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
#define Chunk_block(c) (((heap_chunk_head *) (c)) [-1]).block

extern int llama_gc_phase;
extern int llama_gc_subphase;
extern uintnat llama_allocated_words;
extern double llama_extra_heap_resources;
extern uintnat llama_dependent_size, llama_dependent_allocated;
extern uintnat llama_fl_size_at_phase_change;

#define Phase_mark 0
#define Phase_sweep 1
#define Phase_idle 2
#define Subphase_main 10
#define Subphase_weak1 11
#define Subphase_weak2 12
#define Subphase_final 13

CAMLextern char *llama_heap_start;
extern uintnat total_heap_size;
extern char *llama_gc_sweep_hp;

void llama_init_major_heap (asize_t);           /* size in bytes */
asize_t llama_round_heap_chunk_size (asize_t);  /* size in bytes */
void llama_darken (value, value *);
intnat llama_major_collection_slice (long);
void major_collection (void);
void llama_finish_major_cycle (void);


#endif /* LLAMA_MAJOR_GC_H */
