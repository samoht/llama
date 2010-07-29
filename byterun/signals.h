/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: signals.h 7904 2007-02-23 09:29:45Z xleroy $ */

#ifndef CAML_SIGNALS_H
#define CAML_SIGNALS_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

/* <private> */
CAMLextern intnat volatile llama_signals_are_pending;
CAMLextern intnat volatile llama_pending_signals[];
CAMLextern int volatile llama_something_to_do;
extern int volatile llama_force_major_slice;
/* </private> */

CAMLextern void llama_enter_blocking_section (void);
CAMLextern void llama_leave_blocking_section (void);

/* <private> */
void llama_urge_major_slice (void);
CAMLextern int llama_convert_signal_number (int);
CAMLextern int llama_rev_convert_signal_number (int);
void llama_execute_signal(int signal_number, int in_signal_handler);
void llama_record_signal(int signal_number);
void llama_process_pending_signals(void);
void llama_process_event(void);
int llama_set_signal_action(int signo, int action);

CAMLextern void (*llama_enter_blocking_section_hook)(void);
CAMLextern void (*llama_leave_blocking_section_hook)(void);
CAMLextern int (*llama_try_leave_blocking_section_hook)(void);
CAMLextern void (* volatile llama_async_action_hook)(void);
/* </private> */

#endif /* CAML_SIGNALS_H */
