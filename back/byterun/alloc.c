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

/* $Id: alloc.c 7849 2007-02-09 13:31:15Z doligez $ */

/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include <string.h>
#include "alloc.h"
#include "custom.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "stacks.h"

#define Setup_for_gc
#define Restore_after_gc

CAMLexport value llama_alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  Assert (tag < 256);
  Assert (tag != Infix_tag);
  if (wosize == 0){
    result = Atom (tag);
  }else if (wosize <= Max_young_wosize){
    Alloc_small (result, wosize, tag);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = 0;
    }
  }else{
    result = llama_alloc_shr (wosize, tag);
    if (tag < No_scan_tag) memset (Bp_val (result), 0, Bsize_wsize (wosize));
    result = llama_check_urgent_gc (result);
  }
  return result;
}

CAMLexport value llama_alloc_small (mlsize_t wosize, tag_t tag)
{
  value result;

  Assert (wosize > 0);
  Assert (wosize <= Max_young_wosize);
  Assert (tag < 256);
  Alloc_small (result, wosize, tag);
  return result;
}

CAMLexport value llama_alloc_tuple(mlsize_t n)
{
  return llama_alloc(n, 0);
}

CAMLexport value llama_alloc_string (mlsize_t len)
{
  value result;
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);

  if (wosize <= Max_young_wosize) {
    Alloc_small (result, wosize, String_tag);
  }else{
    result = llama_alloc_shr (wosize, String_tag);
    result = llama_check_urgent_gc (result);
  }
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

CAMLexport value llama_alloc_final (mlsize_t len, final_fun fun,
                                   mlsize_t mem, mlsize_t max)
{
  return llama_alloc_custom(llama_final_custom_operations(fun),
                           len * sizeof(value), mem, max);
}

CAMLexport value llama_copy_string(char const *s)
{
  int len;
  value res;

  len = strlen(s);
  res = llama_alloc_string(len);
  memmove(String_val(res), s, len);
  return res;
}

CAMLexport value llama_alloc_array(value (*funct)(char const *),
                                  char const ** arr)
{
  CAMLparam0 ();
  mlsize_t nbr, n;
  CAMLlocal2 (v, result);

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    CAMLreturn (Atom(0));
  } else {
    result = llama_alloc (nbr, 0);
    for (n = 0; n < nbr; n++) {
      /* The two statements below must be separate because of evaluation
         order (don't take the address &Field(result, n) before
         calling funct, which may cause a GC and move result). */
      v = funct(arr[n]);
      llama_modify(&Field(result, n), v);
    }
    CAMLreturn (result);
  }
}

CAMLexport value llama_copy_string_array(char const ** arr)
{
  return llama_alloc_array(llama_copy_string, arr);
}

CAMLexport int llama_convert_flag_list(value list, int *flags)
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_val(Field(list, 0))];
    list = Field(list, 1);
  }
  return res;
}

/* For compiling let rec over values */

CAMLprim value llama_alloc_dummy(value size)
{
  mlsize_t wosize = Int_val(size);

  if (wosize == 0) return Atom(0);
  return llama_alloc (wosize, 0);
}

CAMLprim value llama_alloc_dummy_float (value size)
{
  mlsize_t wosize = Int_val(size) * Double_wosize;

  if (wosize == 0) return Atom(0);
  return llama_alloc (wosize, 0);
}

CAMLprim value llama_update_dummy(value dummy, value newval)
{
  mlsize_t size, i;
  tag_t tag;

  size = Wosize_val(newval);
  tag = Tag_val (newval);
  Assert (size == Wosize_val(dummy));
  Assert (tag < No_scan_tag || tag == Double_array_tag);

  Tag_val(dummy) = tag;
  if (tag == Double_array_tag){
    size = Wosize_val (newval) / Double_wosize;
    for (i = 0; i < size; i++){
      Store_double_field (dummy, i, Double_field (newval, i));
    }
  }else{
    for (i = 0; i < size; i++){
      llama_modify (&Field(dummy, i), Field(newval, i));
    }
  }
  return Val_unit;
}
