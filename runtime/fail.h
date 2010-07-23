#ifndef _fail_
#define _fail_


#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

#define OUT_OF_MEMORY_EXN 0     /* "Exc","Out_of_memory",1 */
#define SYS_ERROR_EXN 1         /* "Sys","Sys_error",1 */
#define FAILURE_EXN 2           /* "Exc","Failure",3 */
#define INVALID_EXN 3           /* "Exc","Invalid_argument",2 */
#define END_OF_FILE_EXN 4       /* "Io","End_of_file",1 */
#define ZERO_DIVIDE_EXN 5       /* "Int","Division_by_zero",1 */
#define BREAK_EXN 6             /* "Sys","Break",2 */
#define NOT_FOUND_EXN 7         /* "Exc","Not_found",4 */
#define UNIX_ERROR_EXN 8        /* "Unix","Unix_error",1 */
#define GRAPHIC_FAILURE_EXN 9   /* "Graphics","Graphic_failure",1 */
#define PARSE_FAILURE_EXN 10    /* "Stream","Parse_failure",1 */
#define TCL_ERROR_EXN 11        /* "Protocol","TkError",1 */

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

void mlraise P((value));
void raise_with_arg P((tag_t tag, value arg));
void raise_with_string P((tag_t tag, char * msg));
void failwith P((char *));
void invalid_argument P((char *));
void raise_out_of_memory P((void));

#endif /* _fail_ */
