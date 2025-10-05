#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include "pkg/noun/allocate_ocaml.h"

typedef struct block_entry {
  void* data;
  mlsize_t length;
  value bigarray;
  struct block_entry* next;
} block_entry;

static block_entry* g_blocks = NULL;

static block_entry*
find_block(void* data)
{
  for ( block_entry* entry = g_blocks; entry; entry = entry->next ) {
    if ( entry->data == data ) {
      return entry;
    }
  }
  return NULL;
}

static void
remove_block(void* data)
{
  block_entry** cur = &g_blocks;
  while ( *cur ) {
    if ( (*cur)->data == data ) {
      block_entry* doomed = *cur;
      *cur = doomed->next;
      caml_remove_generational_global_root(&doomed->bigarray);
      free(doomed);
      return;
    }
    cur = &(*cur)->next;
  }
}

static block_entry*
register_block(void* data, value bigarray, mlsize_t length)
{
  block_entry* entry = find_block(data);
  if ( entry ) {
    caml_remove_generational_global_root(&entry->bigarray);
    entry->data = data;
    entry->bigarray = bigarray;
    entry->length = length;
    caml_register_generational_global_root(&entry->bigarray);
    return entry;
  }

  entry = malloc(sizeof(*entry));
  if ( NULL == entry ) {
    caml_failwith("u3a_ocaml: out of memory");
  }
  entry->data = data;
  entry->length = length;
  entry->bigarray = bigarray;
  entry->next = g_blocks;
  g_blocks = entry;
  caml_register_generational_global_root(&entry->bigarray);
  return entry;
}

static value*
named_function(const char* name)
{
  value* closure = caml_named_value(name);
  if ( NULL == closure ) {
    caml_failwith("u3a_ocaml: missing callback");
  }
  return closure;
}

void
u3a_ocaml_runtime_register(void)
{
  CAMLparam0();
  static int registered = 0;
  if ( !registered ) {
    value* fn = named_function("u3a_ocaml_register_callbacks");
    caml_callback(*fn, Val_unit);
    registered = 1;
  }
  CAMLreturn0;
}

void
u3a_ocaml_init_once(void)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_init_once");
  caml_callback(*fn, Val_unit);
  CAMLreturn0;
}

void
u3a_ocaml_init_heap(c3_w words)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_init_heap");
  caml_callback(*fn, Val_long(words));
  CAMLreturn0;
}

void
u3a_ocaml_drop_heap(c3_w cap, c3_w ear)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_drop_heap");
  caml_callback2(*fn, Val_long(cap), Val_long(ear));
  CAMLreturn0;
}

void
u3a_ocaml_mark_init(void)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_mark_init");
  caml_callback(*fn, Val_unit);
  CAMLreturn0;
}

void
u3a_ocaml_mark_done(void)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_mark_done");
  caml_callback(*fn, Val_unit);
  CAMLreturn0;
}

static void*
alloc_bigarray_block(const char* name, c3_w len)
{
  CAMLparam0();
  CAMLlocal1(res);
  value* fn = named_function(name);
  res = caml_callback(*fn, Val_long(len));
  void* data = Caml_ba_data_val(res);
  mlsize_t words = Caml_ba_array_val(res)->dim[0];
  register_block(data, res, words);
  CAMLreturnT(void*, data);
}

void*
u3a_ocaml_mark_alloc(c3_w len)
{
  return alloc_bigarray_block("u3a_ocaml_mark_alloc", len);
}

void
u3a_ocaml_pack_init(void)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_pack_init");
  caml_callback(*fn, Val_unit);
  CAMLreturn0;
}

void
u3a_ocaml_pack_done(void)
{
  CAMLparam0();
  value* fn = named_function("u3a_ocaml_pack_done");
  caml_callback(*fn, Val_unit);
  CAMLreturn0;
}

void*
u3a_ocaml_pack_alloc(c3_w len)
{
  return alloc_bigarray_block("u3a_ocaml_pack_alloc", len);
}

void*
u3a_ocaml_walloc(c3_w len)
{
  return alloc_bigarray_block("u3a_ocaml_walloc", len);
}

void*
u3a_ocaml_wealloc(void* old_ptr, c3_w len)
{
  CAMLparam0();
  CAMLlocal1(res);
  block_entry* entry = find_block(old_ptr);
  if ( NULL == entry ) {
    caml_failwith("u3a_ocaml_wealloc: unknown block");
  }
  value* fn = named_function("u3a_ocaml_wealloc");
  res = caml_callback2(*fn, entry->bigarray, Val_long(len));
  void* data = Caml_ba_data_val(res);
  mlsize_t words = Caml_ba_array_val(res)->dim[0];
  remove_block(old_ptr);
  register_block(data, res, words);
  CAMLreturnT(void*, data);
}

void
u3a_ocaml_wfree(void* ptr)
{
  CAMLparam0();
  block_entry* entry = find_block(ptr);
  if ( NULL == entry ) {
    CAMLreturn0;
  }
  value* fn = named_function("u3a_ocaml_wfree");
  caml_callback(*fn, entry->bigarray);
  remove_block(ptr);
  CAMLreturn0;
}

void
u3a_ocaml_wtrim(void* ptr, c3_w old_len, c3_w new_len)
{
  CAMLparam0();
  block_entry* entry = find_block(ptr);
  if ( NULL == entry ) {
    CAMLreturn0;
  }
  value* fn = named_function("u3a_ocaml_wtrim");
  caml_callback3(*fn, entry->bigarray, Val_long(old_len), Val_long(new_len));
  entry->length = new_len;
  CAMLreturn0;
}

void*
u3a_ocaml_calloc(c3_w num, c3_w len)
{
  CAMLparam0();
  CAMLlocal1(res);
  value* fn = named_function("u3a_ocaml_calloc");
  res = caml_callback2(*fn, Val_long(num), Val_long(len));
  void* data = Caml_ba_data_val(res);
  mlsize_t words = Caml_ba_array_val(res)->dim[0];
  register_block(data, res, words);
  CAMLreturnT(void*, data);
}

void*
u3a_ocaml_malloc(c3_w bytes)
{
  return alloc_bigarray_block("u3a_ocaml_malloc", bytes);
}

void*
u3a_ocaml_celloc(void)
{
  CAMLparam0();
  CAMLlocal1(res);
  value* fn = named_function("u3a_ocaml_celloc");
  res = caml_callback(*fn, Val_unit);
  void* data = Caml_ba_data_val(res);
  mlsize_t words = Caml_ba_array_val(res)->dim[0];
  register_block(data, res, words);
  CAMLreturnT(void*, data);
}

void
u3a_ocaml_cfree(void* ptr)
{
  u3a_ocaml_wfree(ptr);
}

void*
u3a_ocaml_realloc(void* ptr, c3_w bytes)
{
  if ( NULL == ptr ) {
    return u3a_ocaml_malloc(bytes);
  }
  CAMLparam0();
  CAMLlocal1(res);
  block_entry* entry = find_block(ptr);
  if ( NULL == entry ) {
    caml_failwith("u3a_ocaml_realloc: unknown block");
  }
  value* fn = named_function("u3a_ocaml_realloc");
  res = caml_callback2(*fn, entry->bigarray, Val_long(bytes));
  void* data = Caml_ba_data_val(res);
  mlsize_t words = Caml_ba_array_val(res)->dim[0];
  remove_block(ptr);
  register_block(data, res, words);
  CAMLreturnT(void*, data);
}

void
u3a_ocaml_free(void* ptr)
{
  if ( NULL == ptr ) {
    return;
  }
  u3a_ocaml_wfree(ptr);
}
