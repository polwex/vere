#ifndef U3_ALLOCATE_OCAML_H
#define U3_ALLOCATE_OCAML_H

#include "types.h"

#ifdef __cplusplus
extern "C" {
#endif

void u3a_ocaml_runtime_register(void);
void u3a_ocaml_init_once(void);
void u3a_ocaml_init_heap(c3_w words);
void u3a_ocaml_drop_heap(c3_w cap, c3_w ear);
void u3a_ocaml_mark_init(void);
void u3a_ocaml_mark_done(void);
void* u3a_ocaml_mark_alloc(c3_w len);
void u3a_ocaml_pack_init(void);
void u3a_ocaml_pack_done(void);
void* u3a_ocaml_pack_alloc(c3_w len);
void* u3a_ocaml_walloc(c3_w len);
void* u3a_ocaml_wealloc(void* old_ptr, c3_w len);
void u3a_ocaml_wfree(void* ptr);
void u3a_ocaml_wtrim(void* ptr, c3_w old_len, c3_w new_len);
void* u3a_ocaml_calloc(c3_w num, c3_w len);
void* u3a_ocaml_malloc(c3_w bytes);
void* u3a_ocaml_celloc(void);
void u3a_ocaml_cfree(void* ptr);
void* u3a_ocaml_realloc(void* ptr, c3_w bytes);
void u3a_ocaml_free(void* ptr);

#ifdef __cplusplus
}
#endif

#endif /* U3_ALLOCATE_OCAML_H */
