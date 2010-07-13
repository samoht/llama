/* Convert constants from C headers to Caml */

#include <stdio.h>

#include <mlvalues.h>
#include <exec.h>
#include <instruct.h>

main () {
  printf("(* Code generated by ``constants.c''. *)\n");
  printf("(* Do not modify. *)\n\n");
  printf("let String_tag = %d;;\n", String_tag);
  printf("let Double_tag = %d;;\n", Double_tag);
  printf("let Trailer_size = %d;;\n", TRAILER_SIZE);
  printf("let Exec_magic = %d;;\n", EXEC_MAGIC);
  printf("let Instr_break = `\\%03d`;;\n", BREAK);
/*   printf("let Instr_apply = `\\%03d`;;\n", APPLY); */
/*   printf("let Instr_push_getglobal_apply = `\\%03d`;;\n",
            PUSH_GETGLOBAL_APPLY); */
  return (0);
}
