#ifndef SCHEME_H
#define SCHEME_H

#include <libguile.h>

void init_guile();
SCM call_scheme_function_1(const char* name, SCM arg);
void load_scheme_file(const char* path);

#endif
