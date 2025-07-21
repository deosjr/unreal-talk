#include <libguile.h>
#include <stdio.h>

void init_guile() {
    scm_init_guile();
}

SCM call_scheme_function_1(const char* name, SCM arg) {
    SCM sym = scm_c_lookup(name);
    SCM proc = scm_variable_ref(sym);
    return scm_call_1(proc, arg);
}

void load_scheme_file(const char* path) {
    scm_c_primitive_load(path);
}

