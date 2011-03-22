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

/* $Id: alloc.h 7064 2005-09-22 14:21:50Z xleroy $ */

#ifndef LLAMA_ALLOC_H
#define LLAMA_ALLOC_H


#ifndef LLAMA_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

CAMLextern value llama_alloc (mlsize_t, tag_t);
CAMLextern value llama_alloc_small (mlsize_t, tag_t);
CAMLextern value llama_alloc_tuple (mlsize_t);
CAMLextern value llama_alloc_string (mlsize_t);  /* size in bytes */
CAMLextern value llama_copy_string (char const *);
CAMLextern value llama_copy_string_array (char const **);
CAMLextern value llama_copy_double (double);
CAMLextern value llama_copy_int32 (int32);       /* defined in [ints.c] */
CAMLextern value llama_copy_int64 (int64);       /* defined in [ints.c] */
CAMLextern value llama_copy_nativeint (intnat);  /* defined in [ints.c] */
CAMLextern value llama_alloc_array (value (*funct) (char const *),
                                   char const ** array);

typedef void (*final_fun)(value);
CAMLextern value llama_alloc_final (mlsize_t, /*size in words*/
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int llama_convert_flag_list (value, int *);

#endif /* LLAMA_ALLOC_H */
