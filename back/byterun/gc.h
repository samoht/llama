/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: gc.h 6557 2004-07-19 13:20:06Z xleroy $ */

#ifndef LLAMA_GC_H
#define LLAMA_GC_H


#include "mlvalues.h"

#define Llama_white (0 << 8)
#define Llama_gray  (1 << 8)
#define Llama_blue  (2 << 8)
#define Llama_black (3 << 8)

#define Color_hd(hd) ((color_t) ((hd) & Llama_black))
#define Color_hp(hp) (Color_hd (Hd_hp (hp)))
#define Color_val(val) (Color_hd (Hd_val (val)))

#define Is_white_hd(hd) (Color_hd (hd) == Llama_white)
#define Is_gray_hd(hd) (Color_hd (hd) == Llama_gray)
#define Is_blue_hd(hd) (Color_hd (hd) == Llama_blue)
#define Is_black_hd(hd) (Color_hd (hd) == Llama_black)

#define Whitehd_hd(hd) (((hd)  & ~Llama_black)/*| Llama_white*/)
#define Grayhd_hd(hd)  (((hd)  & ~Llama_black)  | Llama_gray)
#define Blackhd_hd(hd) (((hd)/*& ~Llama_black*/)| Llama_black)
#define Bluehd_hd(hd)  (((hd)  & ~Llama_black)  | Llama_blue)

/* This depends on the layout of the header.  See [mlvalues.h]. */
#define Make_header(wosize, tag, color)                                       \
      (/*Assert ((wosize) <= Max_wosize),*/                                   \
       ((header_t) (((header_t) (wosize) << 10)                               \
                    + (color)                                                 \
                    + (tag_t) (tag)))                                         \
      )

#define Is_white_val(val) (Color_val(val) == Llama_white)
#define Is_gray_val(val) (Color_val(val) == Llama_gray)
#define Is_blue_val(val) (Color_val(val) == Llama_blue)
#define Is_black_val(val) (Color_val(val) == Llama_black)

/* For extern.c */
#define Colornum_hd(hd) ((color_t) (((hd) >> 8) & 3))
#define Coloredhd_hd(hd,colnum) (((hd) & ~Llama_black) | ((colnum) << 8))

#endif /* LLAMA_GC_H */
