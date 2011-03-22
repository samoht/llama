/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: fork.c 10287 2010-04-20 15:47:15Z doligez $ */

#include "../mlvalues.h"
#include "../debugger.h"
#include "unixsupport.h"

CAMLprim value unix_fork(value unit)
{
  int ret;
  ret = fork();
  if (ret == -1) uerror("fork", Nothing);
  if (llama_debugger_in_use)
    if ((llama_debugger_fork_mode && ret == 0) ||
        (!llama_debugger_fork_mode && ret != 0))
      llama_debugger_cleanup_fork();
  return Val_int(ret);
}
