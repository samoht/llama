/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: callback.h 7596 2006-09-11 12:12:24Z doligez $ */

/* Callbacks from C to Caml */

#ifndef LLAMA_CALLBACK_H
#define LLAMA_CALLBACK_H

#ifndef LLAMA_NAME_SPACE
#include "compatibility.h"
#endif
#include "mlvalues.h"

CAMLextern value llama_callback (value closure, value arg);
CAMLextern value llama_callback2 (value closure, value arg1, value arg2);
CAMLextern value llama_callback3 (value closure, value arg1, value arg2,
                                 value arg3);
CAMLextern value llama_callbackN (value closure, int narg, value args[]);

CAMLextern value llama_callback_exn (value closure, value arg);
CAMLextern value llama_callback2_exn (value closure, value arg1, value arg2);
CAMLextern value llama_callback3_exn (value closure,
                                     value arg1, value arg2, value arg3);
CAMLextern value llama_callbackN_exn (value closure, int narg, value args[]);

#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)

CAMLextern value * llama_named_value (char const * name);

CAMLextern value llama_main (char ** argv);
CAMLextern void llama_startup (char ** argv);

CAMLextern int llama_callback_depth;

#endif
