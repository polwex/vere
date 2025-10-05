open Bigarray

(** OCaml re-implementation of the runtime allocation helpers traditionally
    supplied by [pkg/noun/allocate.c]. The module mirrors the API shape needed
    by the C runtime while exposing hooks that make it convenient to integrate
    with OCaml-managed data. *)

(** Opaque handle over the heap/road state. *)
type road

val init_once : unit -> unit
(** Initialise global bookkeeping structures exactly once. *)

val init_heap : ?words:int -> unit -> unit
(** Prepare an OCaml-managed heap of [words] 32-bit cells. The default mirrors a
    4 MiB arena. *)

val drop_heap : cap:int -> ear:int -> unit
(** Release transient accounting associated with the heap. The OCaml
    implementation tracks statistics but does not presently recycle the
    underlying buffers. *)

val road : unit -> road
(** Retrieve the current road, initialising it on demand. *)

val mark_init : unit -> unit
val mark_done : unit -> unit
val mark_alloc : int -> (int32, int32_elt, c_layout) Array1.t

val pack_init : unit -> unit
val pack_done : unit -> unit
val pack_alloc : int -> (int32, int32_elt, c_layout) Array1.t

val waloc : int -> (int32, int32_elt, c_layout) Array1.t
val wealloc : (int32, int32_elt, c_layout) Array1.t -> int -> (int32, int32_elt, c_layout) Array1.t
val wfree : (int32, int32_elt, c_layout) Array1.t -> unit
val wtrim : (int32, int32_elt, c_layout) Array1.t -> old:int -> len:int -> unit

val calloc : int -> int -> (int32, int32_elt, c_layout) Array1.t
val malloc : int -> (int32, int32_elt, c_layout) Array1.t
val celloc : unit -> (int32, int32_elt, c_layout) Array1.t
val cfree : (int32, int32_elt, c_layout) Array1.t -> unit
val realloc : (int32, int32_elt, c_layout) Array1.t option -> int -> (int32, int32_elt, c_layout) Array1.t
val free : (int32, int32_elt, c_layout) Array1.t option -> unit

(** Profiling support. *)
type profile_snapshot = {
  allocations : int;
  frees : int;
  reallocations : int;
  words_allocated : int;
  words_freed : int;
  peak_words : int;
}

val profile : unit -> profile_snapshot
val reset_profile : unit -> unit

val register_allocation_hook : (int -> unit) -> unit
val register_free_hook : (int -> unit) -> unit
val register_reallocation_hook : (int -> int -> unit) -> unit

(** Register all OCaml functions with the C runtime. *)
val register_callbacks : unit -> unit
