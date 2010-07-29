/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: compatibility.h 8953 2008-07-28 11:59:55Z doligez $ */

/* definitions for compatibility with old identifiers */

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

#ifndef CAML_NAME_SPACE

/*
   #define --> CAMLextern  (defined with CAMLexport or CAMLprim)
   (rien)  --> CAMLprim
   g       --> global C identifier
   x       --> special case

   SP* signals the special cases:
   - when the identifier was not simply prefixed with [llama_]
   - when the [llama_] version was already used for something else, and
     was renamed out of the way (watch out for [llama_alloc] and
     [llama_array_bound_error] in *.s)
*/

/* a faire:
   - ui_*   (reverifier que win32.c n'en depend pas)
*/


/* **** alloc.c */
#define alloc llama_alloc /*SP*/
#define alloc_small llama_alloc_small
#define alloc_tuple llama_alloc_tuple
#define alloc_string llama_alloc_string
#define alloc_final llama_alloc_final
#define copy_string llama_copy_string
#define alloc_array llama_alloc_array
#define copy_string_array llama_copy_string_array
#define convert_flag_list llama_convert_flag_list

/* **** array.c */

/* **** backtrace.c */
#define backtrace_active llama_backtrace_active
#define backtrace_pos llama_backtrace_pos
#define backtrace_buffer llama_backtrace_buffer
#define backtrace_last_exn llama_backtrace_last_exn
#define print_exception_backtrace llama_print_exception_backtrace

/* **** callback.c */
#define callback_depth llama_callback_depth
#define callbackN_exn llama_callbackN_exn
#define callback_exn llama_callback_exn
#define callback2_exn llama_callback2_exn
#define callback3_exn llama_callback3_exn
#define callback llama_callback
#define callback2 llama_callback2
#define callback3 llama_callback3
#define callbackN llama_callbackN

/* **** compact.c */

/* **** compare.c */
#define compare_unordered llama_compare_unordered

/* **** custom.c */
#define alloc_custom llama_alloc_custom
#define register_custom_operations llama_register_custom_operations

/* **** debugger.c */

/* **** dynlink.c */

/* **** extern.c */
#define output_val llama_output_val
#define output_value_to_malloc llama_output_value_to_malloc
#define output_value_to_block llama_output_value_to_block
#define serialize_int_1 llama_serialize_int_1
#define serialize_int_2 llama_serialize_int_2
#define serialize_int_4 llama_serialize_int_4
#define serialize_int_8 llama_serialize_int_8
#define serialize_float_4 llama_serialize_float_4
#define serialize_float_8 llama_serialize_float_8
#define serialize_block_1 llama_serialize_block_1
#define serialize_block_2 llama_serialize_block_2
#define serialize_block_4 llama_serialize_block_4
#define serialize_block_8 llama_serialize_block_8
#define serialize_block_float_8 llama_serialize_block_float_8

/* **** fail.c */
#define external_raise llama_external_raise
#define mlraise llama_raise /*SP*/
#define raise_constant llama_raise_constant
#define raise_with_arg llama_raise_with_arg
#define raise_with_string llama_raise_with_string
#define failwith llama_failwith
#define invalid_argument llama_invalid_argument
#define array_bound_error llama_array_bound_error /*SP*/
#define raise_out_of_memory llama_raise_out_of_memory
#define raise_stack_overflow llama_raise_stack_overflow
#define raise_sys_error llama_raise_sys_error
#define raise_end_of_file llama_raise_end_of_file
#define raise_zero_divide llama_raise_zero_divide
#define raise_not_found llama_raise_not_found
#define raise_sys_blocked_io llama_raise_sys_blocked_io
#define init_exceptions llama_init_exceptions
/* **** asmrun/fail.c */
/* **** asmrun/<arch>.s */

/* **** finalise.c */

/* **** fix_code.c */

/* **** floats.c */
/*#define Double_val llama_Double_val             done in mlvalues.h as needed */
/*#define Store_double_val llama_Store_double_val done in mlvalues.h as needed */
#define copy_double llama_copy_double

/* **** freelist.c */

/* **** gc_ctrl.c */

/* **** globroots.c */
#define register_global_root llama_register_global_root
#define remove_global_root llama_remove_global_root

/* **** hash.c */
#define hash_variant llama_hash_variant

/* **** instrtrace.c */

/* **** intern.c */
#define input_val llama_input_val
#define input_val_from_string llama_input_val_from_string
#define input_value_from_malloc llama_input_value_from_malloc
#define input_value_from_block llama_input_value_from_block
#define deserialize_uint_1 llama_deserialize_uint_1
#define deserialize_sint_1 llama_deserialize_sint_1
#define deserialize_uint_2 llama_deserialize_uint_2
#define deserialize_sint_2 llama_deserialize_sint_2
#define deserialize_uint_4 llama_deserialize_uint_4
#define deserialize_sint_4 llama_deserialize_sint_4
#define deserialize_uint_8 llama_deserialize_uint_8
#define deserialize_sint_8 llama_deserialize_sint_8
#define deserialize_float_4 llama_deserialize_float_4
#define deserialize_float_8 llama_deserialize_float_8
#define deserialize_block_1 llama_deserialize_block_1
#define deserialize_block_2 llama_deserialize_block_2
#define deserialize_block_4 llama_deserialize_block_4
#define deserialize_block_8 llama_deserialize_block_8
#define deserialize_block_float_8 llama_deserialize_block_float_8
#define deserialize_error llama_deserialize_error

/* **** interp.c */

/* **** ints.c */
#define int32_ops llama_int32_ops
#define copy_int32 llama_copy_int32
/*#define Int64_val llama_Int64_val   *** done in mlvalues.h as needed */
#define int64_ops llama_int64_ops
#define copy_int64 llama_copy_int64
#define nativeint_ops llama_nativeint_ops
#define copy_nativeint llama_copy_nativeint

/* **** io.c */
#define channel_mutex_free llama_channel_mutex_free
#define channel_mutex_lock llama_channel_mutex_lock
#define channel_mutex_unlock llama_channel_mutex_unlock
#define channel_mutex_unlock_exn llama_channel_mutex_unlock_exn
#define all_opened_channels llama_all_opened_channels
#define open_descriptor_in llama_open_descriptor_in /*SP*/
#define open_descriptor_out llama_open_descriptor_out /*SP*/
#define close_channel llama_close_channel /*SP*/
#define channel_size llama_channel_size /*SP*/
#define channel_binary_mode llama_channel_binary_mode
#define flush_partial llama_flush_partial /*SP*/
#define flush llama_flush /*SP*/
#define putword llama_putword
#define putblock llama_putblock
#define really_putblock llama_really_putblock
#define seek_out llama_seek_out /*SP*/
#define pos_out llama_pos_out /*SP*/
#define do_read llama_do_read
#define refill llama_refill
#define getword llama_getword
#define getblock llama_getblock
#define really_getblock llama_really_getblock
#define seek_in llama_seek_in /*SP*/
#define pos_in llama_pos_in /*SP*/
#define input_scan_line llama_input_scan_line /*SP*/
#define finalize_channel llama_finalize_channel
#define alloc_channel llama_alloc_channel
/*#define Val_file_offset llama_Val_file_offset   *** done in io.h as needed */
/*#define File_offset_val llama_File_offset_val   *** done in io.h as needed */

/* **** lexing.c */

/* **** main.c */
/* *** no change */

/* **** major_gc.c */
#define heap_start llama_heap_start
#define page_table llama_page_table

/* **** md5.c */
#define md5_string llama_md5_string
#define md5_chan llama_md5_chan
#define MD5Init llama_MD5Init
#define MD5Update llama_MD5Update
#define MD5Final llama_MD5Final
#define MD5Transform llama_MD5Transform

/* **** memory.c */
#define alloc_shr llama_alloc_shr
#define initialize llama_initialize
#define modify llama_modify
#define stat_alloc llama_stat_alloc
#define stat_free llama_stat_free
#define stat_resize llama_stat_resize

/* **** meta.c */

/* **** minor_gc.c */
#define young_start llama_young_start
#define young_end llama_young_end
#define young_ptr llama_young_ptr
#define young_limit llama_young_limit
#define ref_table llama_ref_table
#define minor_collection llama_minor_collection
#define check_urgent_gc llama_check_urgent_gc

/* **** misc.c */

/* **** obj.c */

/* **** parsing.c */

/* **** prims.c */

/* **** printexc.c */
#define format_llama_exception llama_format_exception /*SP*/

/* **** roots.c */
#define local_roots llama_local_roots
#define scan_roots_hook llama_scan_roots_hook
#define do_local_roots llama_do_local_roots

/* **** signals.c */
#define pending_signals llama_pending_signals
#define something_to_do llama_something_to_do
#define enter_blocking_section_hook llama_enter_blocking_section_hook
#define leave_blocking_section_hook llama_leave_blocking_section_hook
#define try_leave_blocking_section_hook llama_try_leave_blocking_section_hook
#define async_action_hook llama_async_action_hook
#define enter_blocking_section llama_enter_blocking_section
#define leave_blocking_section llama_leave_blocking_section
#define convert_signal_number llama_convert_signal_number
/* **** asmrun/signals.c */
#define garbage_collection llama_garbage_collection

/* **** stacks.c */
#define stack_low llama_stack_low
#define stack_high llama_stack_high
#define stack_threshold llama_stack_threshold
#define extern_sp llama_extern_sp
#define trapsp llama_trapsp
#define trap_barrier llama_trap_barrier

/* **** startup.c */
#define atom_table llama_atom_table
/* **** asmrun/startup.c */
#define static_data_start llama_static_data_start
#define static_data_end llama_static_data_end

/* **** str.c */
#define string_length llama_string_length

/* **** sys.c */
#define sys_error llama_sys_error
#define sys_exit llama_sys_exit

/* **** terminfo.c */

/* **** unix.c  &  win32.c */
#define search_exe_in_path llama_search_exe_in_path

/* **** weak.c */

/* **** asmcomp/asmlink.ml */

/* **** asmcomp/cmmgen.ml */

/* **** asmcomp/asmlink.ml, asmcomp/cmmgen.ml, asmcomp/compilenv.ml */

/* ************************************************************* */

/* **** otherlibs/bigarray */
#define int8 llama_ba_int8
#define uint8 llama_ba_uint8
#define int16 llama_ba_int16
#define uint16 llama_ba_uint16
#define MAX_NUM_DIMS CAML_BA_MAX_NUM_DIMS
#define llama_bigarray_kind llama_ba_kind
#define BIGARRAY_FLOAT32 CAML_BA_FLOAT32
#define BIGARRAY_FLOAT64 CAML_BA_FLOAT64
#define BIGARRAY_SINT8 CAML_BA_SINT8
#define BIGARRAY_UINT8 CAML_BA_UINT8
#define BIGARRAY_SINT16 CAML_BA_SINT16
#define BIGARRAY_UINT16 CAML_BA_UINT16
#define BIGARRAY_INT32 CAML_BA_INT32
#define BIGARRAY_INT64 CAML_BA_INT64
#define BIGARRAY_CAML_INT CAML_BA_CAML_INT
#define BIGARRAY_NATIVE_INT CAML_BA_NATIVE_INT
#define BIGARRAY_COMPLEX32 CAML_BA_COMPLEX32
#define BIGARRAY_COMPLEX64 CAML_BA_COMPLEX64
#define BIGARRAY_KIND_MASK CAML_BA_KIND_MASK
#define llama_bigarray_layout llama_ba_layout
#define BIGARRAY_C_LAYOUT CAML_BA_C_LAYOUT
#define BIGARRAY_FORTRAN_LAYOUT CAML_BA_FORTRAN_LAYOUT
#define BIGARRAY_LAYOUT_MASK CAML_BA_LAYOUT_MASK
#define llama_bigarray_managed llama_ba_managed
#define BIGARRAY_EXTERNAL CAML_BA_EXTERNAL
#define BIGARRAY_MANAGED CAML_BA_MANAGED
#define BIGARRAY_MAPPED_FILE CAML_BA_MAPPED_FILE
#define BIGARRAY_MANAGED_MASK CAML_BA_MANAGED_MASK
#define llama_bigarray_proxy llama_ba_proxy
#define llama_bigarray llama_ba_array
#define Bigarray_val Caml_ba_array_val
#define Data_bigarray_val Caml_ba_data_val
#define alloc_bigarray llama_ba_alloc
#define alloc_bigarray_dims llama_ba_alloc_dims
#define bigarray_map_file llama_ba_map_file
#define bigarray_unmap_file llama_ba_unmap_file
#define bigarray_element_size llama_ba_element_size
#define bigarray_byte_size llama_ba_byte_size
#define bigarray_deserialize llama_ba_deserialize
#define MAX_BIGARRAY_MEMORY CAML_BA_MAX_MEMORY
#define bigarray_create llama_ba_create
#define bigarray_get_N llama_ba_get_N
#define bigarray_get_1 llama_ba_get_1
#define bigarray_get_2 llama_ba_get_2
#define bigarray_get_3 llama_ba_get_3
#define bigarray_get_generic llama_ba_get_generic
#define bigarray_set_1 llama_ba_set_1
#define bigarray_set_2 llama_ba_set_2
#define bigarray_set_3 llama_ba_set_3
#define bigarray_set_N llama_ba_set_N
#define bigarray_set_generic llama_ba_set_generic
#define bigarray_num_dims llama_ba_num_dims
#define bigarray_dim llama_ba_dim
#define bigarray_kind llama_ba_kind
#define bigarray_layout llama_ba_layout
#define bigarray_slice llama_ba_slice
#define bigarray_sub llama_ba_sub
#define bigarray_blit llama_ba_blit
#define bigarray_fill llama_ba_fill
#define bigarray_reshape llama_ba_reshape
#define bigarray_init llama_ba_init

#endif /* CAML_NAME_SPACE */
#endif /* CAML_COMPATIBILITY_H */
