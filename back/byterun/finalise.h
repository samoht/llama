/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: finalise.h 6047 2004-01-02 19:23:29Z doligez $ */

#ifndef LLAMA_FINALISE_H
#define LLAMA_FINALISE_H

#include "roots.h"

void llama_final_update (void);
void llama_final_do_calls (void);
void llama_final_do_strong_roots (scanning_action f);
void llama_final_do_weak_roots (scanning_action f);
void llama_final_do_young_roots (scanning_action f);
void llama_final_empty_young (void);
value llama_final_register (value f, value v);

#endif /* LLAMA_FINALISE_H */
