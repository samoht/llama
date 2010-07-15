#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "runtime/api.h"

/* ---------------------------------------------------------------------- */
/* Stubs for Meta module.                                                 */
/* ---------------------------------------------------------------------- */

CAMLprim value caml_zebra_init(value argv) {
    CAMLparam1(argv);
    int n = Wosize_val(argv);
    int i;
    char** arr = (char**)malloc((n+1)*sizeof(char*));
    for (i = 0; i < n; i++) {
        arr[i] = String_val(Field(argv, i));
    }
    arr[n] = NULL;
    zebra_init(arr);
    free((void*)arr);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_zebra_count_globals() {
    return Val_long(zebra_count_globals());
}

CAMLprim value caml_zebra_get_global(value i) {
    return zebra_get_global(Long_val(i));
}

CAMLprim value caml_zebra_set_global(value i, value v) {
    zebra_set_global(Long_val(i), v);
    return Val_unit;
}

CAMLprim void caml_zebra_realloc_globals(value n) {
    zebra_realloc_globals(Long_val(n));
}

CAMLprim value caml_zebra_stat_alloc(value nval) {
    return zebra_stat_alloc(Long_val(nval));
}

CAMLprim value caml_zebra_stat_free(value v) {
    zebra_stat_free(v);
    return Val_unit;
}

CAMLprim value caml_zebra_stat_resize(value v, value nval) {
    return zebra_stat_resize(v, Long_val(nval));
}

CAMLprim value caml_zebra_interpret(value prog, value offset, value len) {
    return zebra_interpret(prog, Long_val(offset), Long_val(len));
}

CAMLprim value caml_zebra_available_primitives() {
    caml_failwith("caml_zebra_available_primitives: not implemented");
}

/* ---------------------------------------------------------------------- */
/* Stubs for Zebra_obj module.                                            */
/* ---------------------------------------------------------------------- */

CAMLprim value caml_zebra_obj_to_int(value zv) {
    return Val_long(zebra_Long_val(zv));
}

CAMLprim value caml_zebra_obj_to_char(value zv) {
    return Val_long(zebra_Long_val(zv));
}

CAMLprim value caml_zebra_obj_to_float(value zv) {
    return caml_copy_double(zebra_Double_val(zv));
}

CAMLprim value caml_zebra_obj_to_string(value zv) {
    return caml_copy_string(zebra_String_val(zv));
}

CAMLprim value caml_zebra_obj_of_int(value v) {
    return zebra_Val_long(Long_val(v));
}

CAMLprim value caml_zebra_obj_of_char(value v) {
    return zebra_Val_long(Long_val(v));
}

CAMLprim value caml_zebra_obj_of_float(value v) {
    return zebra_copy_double(Double_val(v));
}

CAMLprim value caml_zebra_obj_of_string(value v) {
    return zebra_copy_string(String_val(v));
}

CAMLprim value caml_zebra_obj_is_block(zebra_value zv) {
    return Val_long(zebra_Is_block(zv));
}

CAMLprim value caml_zebra_obj_tag(zebra_value zv) {
    return Val_long(zebra_Tag_val(zv));
}

CAMLprim value caml_zebra_obj_size(value zv) {
    return Val_long(zebra_Wosize_val(zv));
}

CAMLprim zebra_value caml_zebra_obj_field(value zv, value i) {
    return zebra_Field(zv, Long_val(i));
}

CAMLprim value caml_zebra_obj_set_field(value zv, value i, value fzv) {
    zebra_Store_field(zv, Long_val(i), fzv);
    return Val_unit;
}

CAMLprim value caml_zebra_obj_new_block(value tag, value sz) {
    return zebra_obj_new_block(Long_val(sz), Long_val(tag));
}
