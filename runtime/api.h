#include <stddef.h>

typedef long llama_value;

void llama_init(char** argv);

char * llama_stat_alloc(size_t);
void llama_stat_free(char *);
char * llama_stat_resize(char *, size_t);

size_t llama_count_globals();
llama_value llama_get_global(size_t i);
void llama_set_global(size_t i, llama_value v);
void llama_realloc_globals(size_t n);

llama_value llama_interpret(char* prog, size_t offset, size_t len);
char** llama_available_primitives();

long llama_Long_val(llama_value v);
double llama_Double_val(llama_value v);
char* llama_String_val(llama_value v);

llama_value llama_Val_long(long l);
llama_value llama_copy_double(double d);
llama_value llama_copy_string(char* s);

int llama_Is_block(llama_value v);
unsigned int llama_obj_tag(llama_value v);
size_t llama_Wosize_val(llama_value v);
llama_value llama_Field(llama_value v, size_t i);
void llama_Store_field(llama_value v, size_t i, llama_value fv);
llama_value llama_obj_new_block(unsigned int tg, size_t sz);

void llama_set_trace_flag(int b);
