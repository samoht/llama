/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: globroots.h 8828 2008-03-10 19:56:39Z xleroy $ */

/* Registration of global memory roots */

#ifndef LLAMA_GLOBROOTS_H
#define LLAMA_GLOBROOTS_H

#include "mlvalues.h"
#include "roots.h"

void llama_scan_global_roots(scanning_action f);
void llama_scan_global_young_roots(scanning_action f);

#endif /* LLAMA_GLOBROOTS_H */
