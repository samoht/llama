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

/* $Id: signals.c 9547 2010-01-22 12:48:24Z doligez $ */

/* Signal handling, code common to the bytecode and native systems */

#include <signal.h>
#include "alloc.h"
#include "callback.h"
#include "config.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "signals_machdep.h"
#include "sys.h"

#ifndef NSIG
#define NSIG 64
#endif

/* The set of pending signals (received but not yet processed) */

CAMLexport intnat volatile llama_signals_are_pending = 0;
CAMLexport intnat volatile llama_pending_signals[NSIG];

/* Execute all pending signals */

void llama_process_pending_signals(void)
{
  int i;

  if (llama_signals_are_pending) {
    llama_signals_are_pending = 0;
    for (i = 0; i < NSIG; i++) {
      if (llama_pending_signals[i]) {
        llama_pending_signals[i] = 0;
        llama_execute_signal(i, 0);
      }
    }
  }
}

/* Record the delivery of a signal, and arrange for it to be processed
   as soon as possible:
   - in bytecode: via llama_something_to_do, processed in llama_process_event
   - in native-code: by playing with the allocation limit, processed
       in llama_garbage_collection
*/

void llama_record_signal(int signal_number)
{
  llama_pending_signals[signal_number] = 1;
  llama_signals_are_pending = 1;
#ifndef NATIVE_CODE
  llama_something_to_do = 1;
#else
  llama_young_limit = llama_young_end;
#endif
}

/* Management of blocking sections. */

static intnat volatile llama_async_signal_mode = 0;

static void llama_enter_blocking_section_default(void)
{
  Assert (llama_async_signal_mode == 0);
  llama_async_signal_mode = 1;
}

static void llama_leave_blocking_section_default(void)
{
  Assert (llama_async_signal_mode == 1);
  llama_async_signal_mode = 0;
}

static int llama_try_leave_blocking_section_default(void)
{
  intnat res;
  Read_and_clear(res, llama_async_signal_mode);
  return res;
}

CAMLexport void (*llama_enter_blocking_section_hook)(void) =
   llama_enter_blocking_section_default;
CAMLexport void (*llama_leave_blocking_section_hook)(void) =
   llama_leave_blocking_section_default;
CAMLexport int (*llama_try_leave_blocking_section_hook)(void) =
   llama_try_leave_blocking_section_default;

CAMLexport void llama_enter_blocking_section(void)
{
  while (1){
    /* Process all pending signals now */
    llama_process_pending_signals();
    llama_enter_blocking_section_hook ();
    /* Check again for pending signals.
       If none, done; otherwise, try again */
    if (! llama_signals_are_pending) break;
    llama_leave_blocking_section_hook ();
  }
}

CAMLexport void llama_leave_blocking_section(void)
{
  llama_leave_blocking_section_hook ();
  llama_process_pending_signals();
}

/* Execute a signal handler immediately */

static value llama_signal_handlers = 0;

void llama_execute_signal(int signal_number, int in_signal_handler)
{
  value res;
#ifdef POSIX_SIGNALS
  sigset_t sigs;
  /* Block the signal before executing the handler, and record in sigs
     the original signal mask */
  sigemptyset(&sigs);
  sigaddset(&sigs, signal_number);
  sigprocmask(SIG_BLOCK, &sigs, &sigs);
#endif
  res = llama_callback_exn(
           Field(llama_signal_handlers, signal_number),
           Val_int(llama_rev_convert_signal_number(signal_number)));
#ifdef POSIX_SIGNALS
  if (! in_signal_handler) {
    /* Restore the original signal mask */
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  } else if (Is_exception_result(res)) {
    /* Restore the original signal mask and unblock the signal itself */
    sigdelset(&sigs, signal_number);
    sigprocmask(SIG_SETMASK, &sigs, NULL);
  }
#endif
  if (Is_exception_result(res)) llama_raise(Extract_exception(res));
}

/* Arrange for a garbage collection to be performed as soon as possible */

int volatile llama_force_major_slice = 0;

void llama_urge_major_slice (void)
{
  llama_force_major_slice = 1;
#ifndef NATIVE_CODE
  llama_something_to_do = 1;
#else
  llama_young_limit = llama_young_end;
  /* This is only moderately effective on ports that cache [llama_young_limit]
     in a register, since [llama_modify] is called directly, not through
     [llama_c_call], so it may take a while before the register is reloaded
     from [llama_young_limit]. */
#endif
}

/* OS-independent numbering of signals */

#ifndef SIGABRT
#define SIGABRT -1
#endif
#ifndef SIGALRM
#define SIGALRM -1
#endif
#ifndef SIGFPE
#define SIGFPE -1
#endif
#ifndef SIGHUP
#define SIGHUP -1
#endif
#ifndef SIGILL
#define SIGILL -1
#endif
#ifndef SIGINT
#define SIGINT -1
#endif
#ifndef SIGKILL
#define SIGKILL -1
#endif
#ifndef SIGPIPE
#define SIGPIPE -1
#endif
#ifndef SIGQUIT
#define SIGQUIT -1
#endif
#ifndef SIGSEGV
#define SIGSEGV -1
#endif
#ifndef SIGTERM
#define SIGTERM -1
#endif
#ifndef SIGUSR1
#define SIGUSR1 -1
#endif
#ifndef SIGUSR2
#define SIGUSR2 -1
#endif
#ifndef SIGCHLD
#define SIGCHLD -1
#endif
#ifndef SIGCONT
#define SIGCONT -1
#endif
#ifndef SIGSTOP
#define SIGSTOP -1
#endif
#ifndef SIGTSTP
#define SIGTSTP -1
#endif
#ifndef SIGTTIN
#define SIGTTIN -1
#endif
#ifndef SIGTTOU
#define SIGTTOU -1
#endif
#ifndef SIGVTALRM
#define SIGVTALRM -1
#endif
#ifndef SIGPROF
#define SIGPROF -1
#endif

static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};

CAMLexport int llama_convert_signal_number(int signo)
{
  if (signo < 0 && signo >= -(sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}

CAMLexport int llama_rev_convert_signal_number(int signo)
{
  int i;
  for (i = 0; i < sizeof(posix_signals) / sizeof(int); i++)
    if (signo == posix_signals[i]) return -i - 1;
  return signo;
}

/* Installation of a signal handler (as per [Sys.signal]) */

CAMLprim value llama_install_signal_handler(value signal_number, value action)
{
  CAMLparam2 (signal_number, action);
  CAMLlocal1 (res);
  int sig, act, oldact;

  sig = llama_convert_signal_number(Int_val(signal_number));
  if (sig < 0 || sig >= NSIG)
    llama_invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = 0;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = 1;
    break;
  default:                      /* Signal_handle */
    act = 2;
    break;
  }
  oldact = llama_set_signal_action(sig, act);
  switch (oldact) {
  case 0:                       /* was Signal_default */
    res = Val_int(0);
    break;
  case 1:                       /* was Signal_ignore */
    res = Val_int(1);
    break;
  case 2:                       /* was Signal_handle */
    res = llama_alloc_small (1, 0);
    Field(res, 0) = Field(llama_signal_handlers, sig);
    break;
  default:                      /* error in llama_set_signal_action */
    llama_sys_error(NO_ARG);
  }
  if (Is_block(action)) {
    if (llama_signal_handlers == 0) {
      llama_signal_handlers = llama_alloc(NSIG, 0);
      llama_register_global_root(&llama_signal_handlers);
    }
    llama_modify(&Field(llama_signal_handlers, sig), Field(action, 0));
  }
  llama_process_pending_signals();
  CAMLreturn (res);
}
