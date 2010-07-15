#include <stddef.h>

typedef long zebra_value;

void zebra_init(char** argv);

char * zebra_stat_alloc(size_t);
void zebra_stat_free(char *);
char * zebra_stat_resize(char *, size_t);

size_t zebra_count_globals();
zebra_value zebra_get_global(size_t i);
void zebra_set_global(size_t i, zebra_value v);
void zebra_realloc_globals(size_t n);

zebra_value zebra_interpret(char* prog, size_t offset, size_t len);
char** zebra_available_primitives();

long zebra_Long_val(zebra_value v);
double zebra_Double_val(zebra_value v);
char* zebra_String_val(zebra_value v);

zebra_value zebra_Val_long(long l);
zebra_value zebra_copy_double(double d);
zebra_value zebra_copy_string(char* s);

int zebra_Is_block(zebra_value v);
unsigned int zebra_Tag_val(zebra_value v);
size_t zebra_Wosize_val(zebra_value v);
zebra_value zebra_Field(zebra_value v, size_t i);
void zebra_Store_field(zebra_value v, size_t i, zebra_value fv);
zebra_value zebra_obj_new_block(unsigned int tg, size_t sz);
