/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: md5.h 9547 2010-01-22 12:48:24Z doligez $ */

/* MD5 message digest */

#ifndef CAML_MD5_H
#define CAML_MD5_H


#include "mlvalues.h"
#include "io.h"

CAMLextern value llama_md5_string (value str, value ofs, value len);
CAMLextern value llama_md5_chan (value vchan, value len);

struct MD5Context {
        uint32 buf[4];
        uint32 bits[2];
        unsigned char in[64];
};

CAMLextern void llama_MD5Init (struct MD5Context *context);
CAMLextern void llama_MD5Update (struct MD5Context *context, unsigned char *buf,
                                uintnat len);
CAMLextern void llama_MD5Final (unsigned char *digest, struct MD5Context *ctx);
CAMLextern void llama_MD5Transform (uint32 *buf, uint32 *in);


#endif /* CAML_MD5_H */
