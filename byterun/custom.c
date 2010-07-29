/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: custom.c 7064 2005-09-22 14:21:50Z xleroy $ */

#include <string.h>

#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

CAMLexport value llama_alloc_custom(struct custom_operations * ops,
                                   uintnat size,
                                   mlsize_t mem,
                                   mlsize_t max)
{
  mlsize_t wosize;
  value result;

  wosize = 1 + (size + sizeof(value) - 1) / sizeof(value);
  if (ops->finalize == NULL && wosize <= Max_young_wosize) {
    result = llama_alloc_small(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
  } else {
    result = llama_alloc_shr(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    llama_adjust_gc_speed(mem, max);
    result = llama_check_urgent_gc(result);
  }
  return result;
}

struct custom_operations_list {
  struct custom_operations * ops;
  struct custom_operations_list * next;
};

static struct custom_operations_list * custom_ops_table = NULL;

CAMLexport void llama_register_custom_operations(struct custom_operations * ops)
{
  struct custom_operations_list * l =
    llama_stat_alloc(sizeof(struct custom_operations_list));
  Assert(ops->identifier != NULL);
  Assert(ops->deserialize != NULL);
  l->ops = ops;
  l->next = custom_ops_table;
  custom_ops_table = l;
}

struct custom_operations * llama_find_custom_operations(char * ident)
{
  struct custom_operations_list * l;
  for (l = custom_ops_table; l != NULL; l = l->next)
    if (strcmp(l->ops->identifier, ident) == 0) return l->ops;
  return NULL;
}

static struct custom_operations_list * custom_ops_final_table = NULL;

struct custom_operations * llama_final_custom_operations(final_fun fn)
{
  struct custom_operations_list * l;
  struct custom_operations * ops;
  for (l = custom_ops_final_table; l != NULL; l = l->next)
    if (l->ops->finalize == fn) return l->ops;
  ops = llama_stat_alloc(sizeof(struct custom_operations));
  ops->identifier = "_final";
  ops->finalize = fn;
  ops->compare = custom_compare_default;
  ops->hash = custom_hash_default;
  ops->serialize = custom_serialize_default;
  ops->deserialize = custom_deserialize_default;
  l = llama_stat_alloc(sizeof(struct custom_operations_list));
  l->ops = ops;
  l->next = custom_ops_final_table;
  custom_ops_final_table = l;
  return ops;
}

extern struct custom_operations llama_int32_ops,
                                llama_nativeint_ops,
                                llama_int64_ops;

void llama_init_custom_operations(void)
{
  llama_register_custom_operations(&llama_int32_ops);
  llama_register_custom_operations(&llama_nativeint_ops);
  llama_register_custom_operations(&llama_int64_ops);
}
