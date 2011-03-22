/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: startup.h 6130 2004-02-22 15:07:51Z xleroy $ */

#ifndef LLAMA_STARTUP_H
#define LLAMA_STARTUP_H

#include "mlvalues.h"
#include "exec.h"

CAMLextern value llama_main(char **argv);

CAMLextern void llama_startup_code(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           char **argv);

enum { FILE_NOT_FOUND = -1, BAD_BYTECODE  = -2 };

extern int llama_attempt_open(char **name, struct exec_trailer *trail,
                             int do_open_script);
extern void llama_read_section_descriptors(int fd, struct exec_trailer *trail);
extern int32 llama_seek_optional_section(int fd, struct exec_trailer *trail,
                                        char *name);
extern int32 llama_seek_section(int fd, struct exec_trailer *trail, char *name);


#endif /* LLAMA_STARTUP_H */
