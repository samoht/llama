/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: backtrace.c 9547 2010-01-22 12:48:24Z doligez $ */

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "mlvalues.h"
#include "alloc.h"
#include "io.h"
#include "instruct.h"
#include "intext.h"
#include "exec.h"
#include "fix_code.h"
#include "memory.h"
#include "startup.h"
#include "stacks.h"
#include "sys.h"
#include "backtrace.h"

CAMLexport int llama_backtrace_active = 0;
CAMLexport int llama_backtrace_pos = 0;
CAMLexport code_t * llama_backtrace_buffer = NULL;
CAMLexport value llama_backtrace_last_exn = Val_unit;
CAMLexport char * llama_cds_file = NULL;
#define BACKTRACE_BUFFER_SIZE 1024

/* Location of fields in the Instruct.debug_event record */
enum { EV_POS = 0,
       EV_MODULE = 1,
       EV_LOC = 2,
       EV_KIND = 3 };

/* Location of fields in the Location.t record. */
enum { LOC_START = 0,
       LOC_END = 1,
       LOC_GHOST = 2 };

/* Location of fields in the Lexing.position record. */
enum {
  POS_FNAME = 0,
  POS_LNUM = 1,
  POS_BOL = 2,
  POS_CNUM = 3
};

/* Start or stop the backtrace machinery */

CAMLprim value llama_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != llama_backtrace_active) {
    llama_backtrace_active = flag;
    llama_backtrace_pos = 0;
    if (flag) {
      llama_register_global_root(&llama_backtrace_last_exn);
    } else {
      llama_remove_global_root(&llama_backtrace_last_exn);
    }
    /* Note: lazy initialization of llama_backtrace_buffer in
       llama_stash_backtrace to simplify the interface with the thread
       libraries */
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value llama_backtrace_status(value vunit)
{
  return Val_bool(llama_backtrace_active);
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void llama_stash_backtrace(value exn, code_t pc, value * sp)
{
  code_t end_code = (code_t) ((char *) llama_start_code + llama_code_size);
  if (pc != NULL) pc = pc - 1;
  if (exn != llama_backtrace_last_exn) {
    llama_backtrace_pos = 0;
    llama_backtrace_last_exn = exn;
  }
  if (llama_backtrace_buffer == NULL) {
    llama_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (llama_backtrace_buffer == NULL) return;
  }
  if (llama_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  if (pc >= llama_start_code && pc < end_code){
    llama_backtrace_buffer[llama_backtrace_pos++] = pc;
  }
  for (/*nothing*/; sp < llama_trapsp; sp++) {
    code_t p = (code_t) *sp;
    if (p >= llama_start_code && p < end_code) {
      if (llama_backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
      llama_backtrace_buffer[llama_backtrace_pos++] = p;
    }
  }
}

/* Read the debugging info contained in the current bytecode executable.
   Return a Caml array of Caml lists of debug_event records in "events",
   or Val_false on failure. */

#ifndef O_BINARY
#define O_BINARY 0
#endif

static value read_debug_info(void)
{
  CAMLparam0();
  CAMLlocal1(events);
  char * exec_name;
  int fd;
  struct exec_trailer trail;
  struct channel * chan;
  uint32 num_events, orig, i;
  value evl, l;

  if (llama_cds_file != NULL) {
    exec_name = llama_cds_file;
  } else {
    exec_name = llama_exe_name;
  }
  fd = llama_attempt_open(&exec_name, &trail, 1);
  if (fd < 0) CAMLreturn(Val_false);
  llama_read_section_descriptors(fd, &trail);
  if (llama_seek_optional_section(fd, &trail, "DBUG") == -1) {
    close(fd);
    CAMLreturn(Val_false);
  }
  chan = llama_open_descriptor_in(fd);
  num_events = llama_getword(chan);
  events = llama_alloc(num_events, 0);
  for (i = 0; i < num_events; i++) {
    orig = llama_getword(chan);
    evl = llama_input_val(chan);
    /* Relocate events in event list */
    for (l = evl; l != Val_int(0); l = Field(l, 1)) {
      value ev = Field(l, 0);
      Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) + orig);
    }
    /* Record event list */
    Store_field(events, i, evl);
  }
  llama_close_channel(chan);
  CAMLreturn(events);
}

/* Search the event for the given PC.  Return Val_false if not found. */

static value event_for_location(value events, code_t pc)
{
  mlsize_t i;
  value pos, l, ev, ev_pos, best_ev;

  best_ev = 0;
  Assert(pc >= llama_start_code && pc < llama_start_code + llama_code_size);
  pos = Val_long((char *) pc - (char *) llama_start_code);
  for (i = 0; i < Wosize_val(events); i++) {
    for (l = Field(events, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);
      ev_pos = Field(ev, EV_POS);
      if (ev_pos == pos) return ev;
      /* ocamlc sometimes moves an event past a following PUSH instruction;
         allow mismatch by 1 instruction. */
      if (ev_pos == pos + 8) best_ev = ev;
    }
  }
  if (best_ev != 0) return best_ev;
  return Val_false;
}

/* Extract location information for the given PC */

struct loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

static void extract_location_info(value events, code_t pc,
                                  /*out*/ struct loc_info * li)
{
  value ev, ev_start;

  ev = event_for_location(events, pc);
  li->loc_is_raise = llama_is_instruction(*pc, RAISE);
  if (ev == Val_false) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;
  ev_start = Field (Field (ev, EV_LOC), LOC_START);
  li->loc_filename = String_val (Field (ev_start, POS_FNAME));
  li->loc_lnum = Int_val (Field (ev_start, POS_LNUM));
  li->loc_startchr =
    Int_val (Field (ev_start, POS_CNUM))
    - Int_val (Field (ev_start, POS_BOL));
  li->loc_endchr =
    Int_val (Field (Field (Field (ev, EV_LOC), LOC_END), POS_CNUM))
    - Int_val (Field (ev_start, POS_BOL));
}

/* Print location information */

static void print_location(struct loc_info * li, int index)
{
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise) return;

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    if (index == 0)
      info = "Raised by primitive operation at";
    else
      info = "Called from";
  }
  if (! li->loc_valid) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    fprintf (stderr, "%s file \"%s\", line %d, characters %d-%d\n",
             info, li->loc_filename, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */

CAMLexport void llama_print_exception_backtrace(void)
{
  value events;
  int i;
  struct loc_info li;

  events = read_debug_info();
  if (events == Val_false) {
    fprintf(stderr,
            "(Program not linked with -g, cannot print stack backtrace)\n");
    return;
  }
  for (i = 0; i < llama_backtrace_pos; i++) {
    extract_location_info(events, llama_backtrace_buffer[i], &li);
    print_location(&li, i);
  }
}

/* Convert the backtrace to a data structure usable from Caml */

CAMLprim value llama_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal5(events, res, arr, p, fname);
  int i;
  struct loc_info li;

  events = read_debug_info();
  if (events == Val_false) {
    res = Val_int(0);           /* None */
  } else {
    arr = llama_alloc(llama_backtrace_pos, 0);
    for (i = 0; i < llama_backtrace_pos; i++) {
      extract_location_info(events, llama_backtrace_buffer[i], &li);
      if (li.loc_valid) {
        fname = llama_copy_string(li.loc_filename);
        p = llama_alloc_small(5, 0);
        Field(p, 0) = Val_bool(li.loc_is_raise);
        Field(p, 1) = fname;
        Field(p, 2) = Val_int(li.loc_lnum);
        Field(p, 3) = Val_int(li.loc_startchr);
        Field(p, 4) = Val_int(li.loc_endchr);
      } else {
        p = llama_alloc_small(1, 1);
        Field(p, 0) = Val_bool(li.loc_is_raise);
      }
      llama_modify(&Field(arr, i), p);
    }
    res = llama_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  }
  CAMLreturn(res);
}
