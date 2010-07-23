/* ML global variables reachable from C. */

#ifndef _globals_
#define _globals_


#include "mlvalues.h"

extern value global_data;

#define GLOBAL_DATA 0           /* "Meta","global_data" */
#define SYS__S_IRUSR 1          /* "Sys","s_irusr" */
#define SYS__S_IWUSR 2          /* "Sys","s_iwusr" */
#define SYS__S_IXUSR 3          /* "Sys","s_ixusr" */
#define SYS__S_IRGRP 4          /* "Sys","s_irgrp" */
#define SYS__S_IWGRP 5          /* "Sys","s_iwgrp" */
#define SYS__S_IXGRP 6          /* "Sys","s_ixgrp" */
#define SYS__S_IROTH 7          /* "Sys","s_iroth" */
#define SYS__S_IWOTH 8          /* "Sys","s_iwoth" */
#define SYS__S_IXOTH 9          /* "Sys","s_ixoth" */
#define SYS__S_ISUID 10         /* "Sys","s_isuid" */
#define SYS__S_ISGID 11         /* "Sys","s_isgid" */
#define SYS__S_IRALL 12         /* "Sys","s_irall" */
#define SYS__S_IWALL 13         /* "Sys","s_iwall" */
#define SYS__S_IXALL 14         /* "Sys","s_ixall" */
#define SYS__COMMAND_LINE 15    /* "Sys","command_line" */
#define SYS__INTERACTIVE 16     /* "Sys","interactive" */
#define SYS__MAX_STRING_LENGTH 17 /* "Sys","max_string_length" */
#define SYS__MAX_VECT_LENGTH 18   /* "Sys","max_vect_length" */
#define SYS__WORD_SIZE 19       /* "Sys","word_size" */

#endif /* _globals_ */
