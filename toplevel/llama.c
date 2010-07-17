#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "runtime/api.h"

/* ---------------------------------------------------------------------- */
/* Stubs for Meta module.                                                 */
/* ---------------------------------------------------------------------- */

CAMLprim value caml_llama_init(value argv) {
    CAMLparam1(argv);
    int n = Wosize_val(argv);
    int i;
    char** arr = (char**)malloc((n+1)*sizeof(char*));
    for (i = 0; i < n; i++) {
        arr[i] = String_val(Field(argv, i));
    }
    arr[n] = NULL;
    llama_init(arr);
    free((void*)arr);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_llama_count_globals(value unit) {
    return Val_long(llama_count_globals());
}

CAMLprim value caml_llama_get_global(value i) {
    return llama_get_global(Long_val(i));
}

CAMLprim value caml_llama_set_global(value i, value v) {
    llama_set_global(Long_val(i), v);
    return Val_unit;
}

CAMLprim void caml_llama_realloc_globals(value n) {
    llama_realloc_globals(Long_val(n));
}

CAMLprim value caml_llama_stat_alloc(value nval) {
    return llama_stat_alloc(Long_val(nval));
}

CAMLprim value caml_llama_stat_free(value v) {
    llama_stat_free(v);
    return Val_unit;
}

CAMLprim value caml_llama_stat_resize(value v, value nval) {
    return llama_stat_resize(v, Long_val(nval));
}

CAMLprim value caml_llama_interpret(value prog, value offset, value len) {
    return llama_interpret(prog, Long_val(offset), Long_val(len));
}

CAMLprim value caml_llama_available_primitives(value unit) {
    return caml_copy_string_array(llama_available_primitives());
}

/* ---------------------------------------------------------------------- */
/* Stubs for Llama_obj module.                                            */
/* ---------------------------------------------------------------------- */

CAMLprim value caml_llama_obj_to_int(value zv) {
    return Val_long(llama_Long_val(zv));
}

CAMLprim value caml_llama_obj_to_char(value zv) {
    return Val_long(llama_Long_val(zv));
}

CAMLprim value caml_llama_obj_to_float(value zv) {
    return caml_copy_double(llama_Double_val(zv));
}

CAMLprim value caml_llama_obj_to_string(value zv) {
    return caml_copy_string(llama_String_val(zv));
}

CAMLprim value caml_llama_obj_of_int(value v) {
    return llama_Val_long(Long_val(v));
}

CAMLprim value caml_llama_obj_of_char(value v) {
    return llama_Val_long(Long_val(v));
}

CAMLprim value caml_llama_obj_of_float(value v) {
    return llama_copy_double(Double_val(v));
}

CAMLprim value caml_llama_obj_of_string(value v) {
    return llama_copy_string(String_val(v));
}

CAMLprim value caml_llama_obj_is_block(llama_value zv) {
    return Val_long(llama_Is_block(zv));
}

CAMLprim value caml_llama_obj_tag(llama_value zv) {
    return Val_long(llama_obj_tag(zv));
}

CAMLprim value caml_llama_obj_size(value zv) {
    return Val_long(llama_Wosize_val(zv));
}

CAMLprim llama_value caml_llama_obj_field(value zv, value i) {
    return llama_Field(zv, Long_val(i));
}

CAMLprim value caml_llama_obj_set_field(value zv, value i, value fzv) {
    llama_Store_field(zv, Long_val(i), fzv);
    return Val_unit;
}

CAMLprim value caml_llama_obj_new_block(value tag, value sz) {
    return llama_obj_new_block(Long_val(tag), Long_val(sz));
}

CAMLprim value caml_llama_set_trace_flag(value b) {
    llama_set_trace_flag(Long_val(b));
    return Val_unit;
}
