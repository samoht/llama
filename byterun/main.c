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

/* $Id: main.c 8822 2008-02-29 12:56:15Z doligez $ */

/* Main entry point (can be overridden by a user-provided main()
   function that calls llama_main() later). */

#include "misc.h"
#include "mlvalues.h"
#include "sys.h"

CAMLextern void llama_main (char **);

#ifdef _WIN32
CAMLextern void llama_expand_command_line (int *, char ***);
#endif

int main(int argc, char **argv)
{
#ifdef DEBUG
  {
    char *ocp;
    char *cp;
    int i;

    llama_gc_message (-1, "### OCaml runtime: debug mode ###\n", 0);
#if 0
    llama_gc_message (-1, "### command line:", 0);
    for (i = 0; i < argc; i++){
      llama_gc_message (-1, " %s", argv[i]);
    }
    llama_gc_message (-1, "\n", 0);
    ocp = getenv ("OCAMLRUNPARAM");
    llama_gc_message (-1, "### OCAMLRUNPARAM=%s\n", ocp == NULL ? "" : ocp);
    cp = getenv ("CAMLRUNPARAM");
    llama_gc_message (-1, "### CAMLRUNPARAM=%s\n", cp == NULL ? "" : cp);
    llama_gc_message (-1, "### working dir: %s\n", getcwd (NULL, 0));
#endif
  }
#endif
#ifdef _WIN32
  /* Expand wildcards and diversions in command line */
  llama_expand_command_line(&argc, &argv);
#endif
  llama_main(argv);
  llama_sys_exit(Val_int(0));
  return 0; /* not reached */
}
