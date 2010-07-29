/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: debugger.c 10287 2010-04-20 15:47:15Z doligez $ */

/* Interface with the byte-code debugger */

#ifdef _WIN32
#include <io.h>
#endif /* _WIN32 */

#include <string.h>

#include "config.h"
#include "debugger.h"
#include "misc.h"

int llama_debugger_in_use = 0;
uintnat llama_event_count;
int llama_debugger_fork_mode = 1; /* parent by default */

#if !defined(HAS_SOCKETS) || defined(NATIVE_CODE)

void llama_debugger_init(void)
{
}

void llama_debugger(enum event_kind event)
{
}

void llama_debugger_cleanup_fork(void)
{
}

#else

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#ifndef _WIN32
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#else
#define ATOM ATOM_WS
#include <winsock.h>
#undef ATOM
#include <process.h>
#endif

#include "fail.h"
#include "fix_code.h"
#include "instruct.h"
#include "intext.h"
#include "io.h"
#include "mlvalues.h"
#include "stacks.h"
#include "sys.h"

static int sock_domain;         /* Socket domain for the debugger */
static union {                  /* Socket address for the debugger */
  struct sockaddr s_gen;
#ifndef _WIN32
  struct sockaddr_un s_unix;
#endif
  struct sockaddr_in s_inet;
} sock_addr;
static int sock_addr_len;       /* Length of sock_addr */

static int dbg_socket = -1;     /* The socket connected to the debugger */
static struct channel * dbg_in; /* Input channel on the socket */
static struct channel * dbg_out;/* Output channel on the socket */

static char *dbg_addr = "(none)";

static void open_connection(void)
{
#ifdef _WIN32
  /* Set socket to synchronous mode so that file descriptor-oriented
     functions (read()/write() etc.) can be used */

  int oldvalue, oldvaluelen, newvalue, retcode;
  oldvaluelen = sizeof(oldvalue);
  retcode = getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                       (char *) &oldvalue, &oldvaluelen);
  if (retcode == 0) {
      newvalue = SO_SYNCHRONOUS_NONALERT;
      setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                 (char *) &newvalue, sizeof(newvalue));
  }
#endif
  dbg_socket = socket(sock_domain, SOCK_STREAM, 0);
#ifdef _WIN32
  if (retcode == 0) {
    /* Restore initial mode */
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
               (char *) &oldvalue, oldvaluelen);
  }
#endif
  if (dbg_socket == -1 ||
      connect(dbg_socket, &sock_addr.s_gen, sock_addr_len) == -1){
    llama_fatal_error_arg2 ("cannot connect to debugger at %s\n", dbg_addr,
                           "error: %s\n", strerror (errno));
  }
#ifdef _WIN32
  dbg_socket = _open_osfhandle(dbg_socket, 0);
  if (dbg_socket == -1)
    llama_fatal_error("_open_osfhandle failed");
#endif
  dbg_in = llama_open_descriptor_in(dbg_socket);
  dbg_out = llama_open_descriptor_out(dbg_socket);
  if (!llama_debugger_in_use) llama_putword(dbg_out, -1); /* first connection */
#ifdef _WIN32
  llama_putword(dbg_out, _getpid());
#else
  llama_putword(dbg_out, getpid());
#endif
  llama_flush(dbg_out);
}

static void close_connection(void)
{
  llama_close_channel(dbg_in);
  llama_close_channel(dbg_out);
  dbg_socket = -1;              /* was closed by llama_close_channel */
}

#ifdef _WIN32
static void winsock_startup(void)
{
  WSADATA wsaData;
  int err = WSAStartup(MAKEWORD(2, 0), &wsaData);
  if (err) llama_fatal_error("WSAStartup failed");
}

static void winsock_cleanup(void)
{
  WSACleanup();
}
#endif

void llama_debugger_init(void)
{
  char * address;
  char * port, * p;
  struct hostent * host;
  int n;

  address = getenv("CAML_DEBUG_SOCKET");
  if (address == NULL) return;
  dbg_addr = address;

#ifdef _WIN32
  winsock_startup();
  (void)atexit(winsock_cleanup);
#endif
  /* Parse the address */
  port = NULL;
  for (p = address; *p != 0; p++) {
    if (*p == ':') { *p = 0; port = p+1; break; }
  }
  if (port == NULL) {
#ifndef _WIN32
    /* Unix domain */
    sock_domain = PF_UNIX;
    sock_addr.s_unix.sun_family = AF_UNIX;
    strncpy(sock_addr.s_unix.sun_path, address,
            sizeof(sock_addr.s_unix.sun_path));
    sock_addr_len =
      ((char *)&(sock_addr.s_unix.sun_path) - (char *)&(sock_addr.s_unix))
        + strlen(address);
#else
    llama_fatal_error("Unix sockets not supported");
#endif
  } else {
    /* Internet domain */
    sock_domain = PF_INET;
    for (p = (char *) &sock_addr.s_inet, n = sizeof(sock_addr.s_inet);
         n > 0; n--) *p++ = 0;
    sock_addr.s_inet.sin_family = AF_INET;
    sock_addr.s_inet.sin_addr.s_addr = inet_addr(address);
    if (sock_addr.s_inet.sin_addr.s_addr == -1) {
      host = gethostbyname(address);
      if (host == NULL)
        llama_fatal_error_arg("Unknown debugging host %s\n", address);
      memmove(&sock_addr.s_inet.sin_addr, host->h_addr, host->h_length);
    }
    sock_addr.s_inet.sin_port = htons(atoi(port));
    sock_addr_len = sizeof(sock_addr.s_inet);
  }
  open_connection();
  llama_debugger_in_use = 1;
  llama_trap_barrier = llama_stack_high;
}

static value getval(struct channel *chan)
{
  value res;
  if (llama_really_getblock(chan, (char *) &res, sizeof(res)) == 0)
    llama_raise_end_of_file(); /* Bad, but consistent with llama_getword */
  return res;
}

static void putval(struct channel *chan, value val)
{
  llama_really_putblock(chan, (char *) &val, sizeof(val));
}

static void safe_output_value(struct channel *chan, value val)
{
  struct longjmp_buffer raise_buf, * saved_external_raise;

  /* Catch exceptions raised by [llama_output_val] */
  saved_external_raise = llama_external_raise;
  if (sigsetjmp(raise_buf.buf, 0) == 0) {
    llama_external_raise = &raise_buf;
    llama_output_val(chan, val, Val_unit);
  } else {
    /* Send wrong magic number, will cause [llama_input_value] to fail */
    llama_really_putblock(chan, "\000\000\000\000", 4);
  }
  llama_external_raise = saved_external_raise;
}

#define Pc(sp) ((code_t)((sp)[0]))
#define Env(sp) ((sp)[1])
#define Extra_args(sp) (Long_val(((sp)[2])))
#define Locals(sp) ((sp) + 3)

void llama_debugger(enum event_kind event)
{
  int frame_number;
  value * frame;
  intnat i, pos;
  value val;

  if (dbg_socket == -1) return;  /* Not connected to a debugger. */

  /* Reset current frame */
  frame_number = 0;
  frame = llama_extern_sp + 1;

  /* Report the event to the debugger */
  switch(event) {
  case PROGRAM_START:           /* Nothing to report */
    goto command_loop;
  case EVENT_COUNT:
    putch(dbg_out, REP_EVENT);
    break;
  case BREAKPOINT:
    putch(dbg_out, REP_BREAKPOINT);
    break;
  case PROGRAM_EXIT:
    putch(dbg_out, REP_EXITED);
    break;
  case TRAP_BARRIER:
    putch(dbg_out, REP_TRAP);
    break;
  case UNCAUGHT_EXC:
    putch(dbg_out, REP_UNCAUGHT_EXC);
    break;
  }
  llama_putword(dbg_out, llama_event_count);
  if (event == EVENT_COUNT || event == BREAKPOINT) {
    llama_putword(dbg_out, llama_stack_high - frame);
    llama_putword(dbg_out, (Pc(frame) - llama_start_code) * sizeof(opcode_t));
  } else {
    /* No PC and no stack frame associated with other events */
    llama_putword(dbg_out, 0);
    llama_putword(dbg_out, 0);
  }
  llama_flush(dbg_out);

 command_loop:

  /* Read and execute the commands sent by the debugger */
  while(1) {
    switch(getch(dbg_in)) {
    case REQ_SET_EVENT:
      pos = llama_getword(dbg_in);
      Assert (pos >= 0);
      Assert (pos < llama_code_size);
      llama_set_instruction(llama_start_code + pos / sizeof(opcode_t), EVENT);
      break;
    case REQ_SET_BREAKPOINT:
      pos = llama_getword(dbg_in);
      Assert (pos >= 0);
      Assert (pos < llama_code_size);
      llama_set_instruction(llama_start_code + pos / sizeof(opcode_t), BREAK);
      break;
    case REQ_RESET_INSTR:
      pos = llama_getword(dbg_in);
      Assert (pos >= 0);
      Assert (pos < llama_code_size);
      pos = pos / sizeof(opcode_t);
      llama_set_instruction(llama_start_code + pos, llama_saved_code[pos]);
      break;
    case REQ_CHECKPOINT:
#ifndef _WIN32
      i = fork();
      if (i == 0) {
        close_connection();     /* Close parent connection. */
        open_connection();      /* Open new connection with debugger */
      } else {
        llama_putword(dbg_out, i);
        llama_flush(dbg_out);
      }
#else
      llama_fatal_error("error: REQ_CHECKPOINT command");
      exit(-1);
#endif
      break;
    case REQ_GO:
      llama_event_count = llama_getword(dbg_in);
      return;
    case REQ_STOP:
      exit(0);
      break;
    case REQ_WAIT:
#ifndef _WIN32
      wait(NULL);
#else
      llama_fatal_error("Fatal error: REQ_WAIT command");
      exit(-1);
#endif
      break;
    case REQ_INITIAL_FRAME:
      frame = llama_extern_sp + 1;
      /* Fall through */
    case REQ_GET_FRAME:
      llama_putword(dbg_out, llama_stack_high - frame);
      if (frame < llama_stack_high){
        llama_putword(dbg_out, (Pc(frame) - llama_start_code) * sizeof(opcode_t));
      }else{
        llama_putword (dbg_out, 0);
      }
      llama_flush(dbg_out);
      break;
    case REQ_SET_FRAME:
      i = llama_getword(dbg_in);
      frame = llama_stack_high - i;
      break;
    case REQ_UP_FRAME:
      i = llama_getword(dbg_in);
      if (frame + Extra_args(frame) + i + 3 >= llama_stack_high) {
        llama_putword(dbg_out, -1);
      } else {
        frame += Extra_args(frame) + i + 3;
        llama_putword(dbg_out, llama_stack_high - frame);
        llama_putword(dbg_out, (Pc(frame) - llama_start_code) * sizeof(opcode_t));
      }
      llama_flush(dbg_out);
      break;
    case REQ_SET_TRAP_BARRIER:
      i = llama_getword(dbg_in);
      llama_trap_barrier = llama_stack_high - i;
      break;
    case REQ_GET_LOCAL:
      i = llama_getword(dbg_in);
      putval(dbg_out, Locals(frame)[i]);
      llama_flush(dbg_out);
      break;
    case REQ_GET_ENVIRONMENT:
      i = llama_getword(dbg_in);
      putval(dbg_out, Field(Env(frame), i));
      llama_flush(dbg_out);
      break;
    case REQ_GET_GLOBAL:
      i = llama_getword(dbg_in);
      putval(dbg_out, Field(llama_global_data, i));
      llama_flush(dbg_out);
      break;
    case REQ_GET_ACCU:
      putval(dbg_out, *llama_extern_sp);
      llama_flush(dbg_out);
      break;
    case REQ_GET_HEADER:
      val = getval(dbg_in);
      llama_putword(dbg_out, Hd_val(val));
      llama_flush(dbg_out);
      break;
    case REQ_GET_FIELD:
      val = getval(dbg_in);
      i = llama_getword(dbg_in);
      if (Tag_val(val) != Double_array_tag) {
        putch(dbg_out, 0);
        putval(dbg_out, Field(val, i));
      } else {
        double d = Double_field(val, i);
        putch(dbg_out, 1);
        llama_really_putblock(dbg_out, (char *) &d, 8);
      }
      llama_flush(dbg_out);
      break;
    case REQ_MARSHAL_OBJ:
      val = getval(dbg_in);
      safe_output_value(dbg_out, val);
      llama_flush(dbg_out);
      break;
    case REQ_GET_CLOSURE_CODE:
      val = getval(dbg_in);
      llama_putword(dbg_out, (Code_val(val)-llama_start_code) * sizeof(opcode_t));
      llama_flush(dbg_out);
      break;
    case REQ_SET_FORK_MODE:
      llama_debugger_fork_mode = llama_getword(dbg_in);
      break;
    }
  }
}

void llama_debugger_cleanup_fork(void)
{
  /* We could remove all of the breakpoints, but closing the connection
   * means that they'll just be skipped anyway. */
  close_connection();
  llama_debugger_in_use = 0;
}

#endif
