open Bigarray

type profile_snapshot = {
  allocations : int;
  frees : int;
  reallocations : int;
  words_allocated : int;
  words_freed : int;
  peak_words : int;
}

module Hooks = struct
  type t = {
    mutable allocation : (int -> unit) list;
    mutable free : (int -> unit) list;
    mutable reallocation : (int -> int -> unit) list;
  }

  let create () = { allocation = []; free = []; reallocation = [] }

  let notify_allocation t words =
    List.iter (fun hook -> hook words) t.allocation

  let notify_free t words =
    List.iter (fun hook -> hook words) t.free

  let notify_reallocation t old_words new_words =
    List.iter (fun hook -> hook old_words new_words) t.reallocation
end

module Profile = struct
  type t = {
    mutable allocations : int;
    mutable frees : int;
    mutable reallocations : int;
    mutable words_allocated : int;
    mutable words_freed : int;
    mutable peak_words : int;
    mutable current_words : int;
  }

  let create () =
    {
      allocations = 0;
      frees = 0;
      reallocations = 0;
      words_allocated = 0;
      words_freed = 0;
      peak_words = 0;
      current_words = 0;
    }

  let record_alloc t words =
    t.allocations <- t.allocations + 1;
    t.words_allocated <- t.words_allocated + words;
    t.current_words <- t.current_words + words;
    if t.current_words > t.peak_words then
      t.peak_words <- t.current_words

  let record_free t words =
    t.frees <- t.frees + 1;
    t.words_freed <- t.words_freed + words;
    t.current_words <- max 0 (t.current_words - words)

  let record_realloc t old_words new_words =
    t.reallocations <- t.reallocations + 1;
    (* count as a free + alloc for aggregate stats *)
    record_free t old_words;
    record_alloc t new_words

  let snapshot t =
    {
      allocations = t.allocations;
      frees = t.frees;
      reallocations = t.reallocations;
      words_allocated = t.words_allocated;
      words_freed = t.words_freed;
      peak_words = t.peak_words;
    }

  let reset t =
    t.allocations <- 0;
    t.frees <- 0;
    t.reallocations <- 0;
    t.words_allocated <- 0;
    t.words_freed <- 0;
    t.peak_words <- t.current_words
end

type mark_state = {
  mutable bitset : (int32, int32_elt, c_layout) Array1.t;
  mutable buffer : (int32, int32_elt, c_layout) Array1.t;
  mutable size : int;
  mutable len : int;
  wee : int array;
}

type pack_state = {
  mutable bitset : (int32, int32_elt, c_layout) Array1.t;
  mutable pap : (int32, int32_elt, c_layout) Array1.t;
  mutable pum : (int32, int32_elt, c_layout) Array1.t;
  mutable buffer : (int32, int32_elt, c_layout) Array1.t;
  mutable size : int;
  mutable len : int;
}

type road = {
  mutable heap_words : int;
  mark : mark_state;
  pack : pack_state;
  profile : Profile.t;
  hooks : Hooks.t;
}

let default_heap_words = 1 lsl 20
let default_mark_size = 1 lsl 12
let default_pack_size = 1 lsl 12
let crag_slots = 1 lsl 6

let empty_words () = Array1.create Int32 C_layout 0

let make_mark_state () =
  {
    bitset = empty_words ();
    buffer = empty_words ();
    size = 0;
    len = 0;
    wee = Array.make crag_slots 0;
  }

let make_pack_state () =
  {
    bitset = empty_words ();
    pap = empty_words ();
    pum = empty_words ();
    buffer = empty_words ();
    size = 0;
    len = 0;
  }

let the_road : road option ref = ref None

let ensure_mark_capacity (state : mark_state) len =
  if len > state.size - state.len then begin
    let need = state.len + len in
    let next = max (state.size * 2) (max default_mark_size need) in
    let resized = Array1.create Int32 C_layout next in
    if state.len > 0 then
      Array1.blit (Array1.sub state.buffer 0 state.len) (Array1.sub resized 0 state.len);
    state.buffer <- resized;
    state.size <- next
  end

let ensure_pack_capacity state len =
  if len > state.size - state.len then begin
    let need = state.len + len in
    let next = max (state.size * 2) (max default_pack_size need) in
    let resized = Array1.create Int32 C_layout next in
    if state.len > 0 then
      Array1.blit (Array1.sub state.buffer 0 state.len) (Array1.sub resized 0 state.len);
    state.buffer <- resized;
    state.size <- next
  end

let make_road () =
  {
    heap_words = default_heap_words;
    mark = make_mark_state ();
    pack = make_pack_state ();
    profile = Profile.create ();
    hooks = Hooks.create ();
  }

let road () =
  match !the_road with
  | Some road -> road
  | None ->
      let road = make_road () in
      the_road := Some road;
      road

let init_once () = ignore (road ())

let init_heap ?(words = default_heap_words) () =
  let road = road () in
  road.heap_words <- words;
  Profile.reset road.profile;
  Array.fill road.mark.wee 0 (Array.length road.mark.wee) 0;
  road.mark.bitset <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.mark.buffer <- Array1.create Int32 C_layout (max default_mark_size words);
  road.mark.size <- Array1.dim road.mark.buffer;
  road.mark.len <- 0;
  road.pack.bitset <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.pack.pap <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.pack.pum <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.pack.buffer <- Array1.create Int32 C_layout (max default_pack_size words);
  road.pack.size <- Array1.dim road.pack.buffer;
  road.pack.len <- 0

let drop_heap ~cap:_ ~ear:_ =
  let road = road () in
  road.mark.len <- 0;
  road.pack.len <- 0

let mark_init () =
  let road = road () in
  let words = road.heap_words in
  road.mark.bitset <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.mark.buffer <- Array1.create Int32 C_layout (max default_mark_size words);
  road.mark.size <- Array1.dim road.mark.buffer;
  road.mark.len <- 0;
  Array1.fill road.mark.bitset Int32.zero;
  Array.fill road.mark.wee 0 (Array.length road.mark.wee) 0

let mark_done () =
  let road = road () in
  road.mark.bitset <- empty_words ();
  road.mark.buffer <- empty_words ();
  road.mark.size <- 0;
  road.mark.len <- 0

let mark_alloc len =
  let road = road () in
  ensure_mark_capacity road.mark len;
  let start = road.mark.len in
  road.mark.len <- start + len;
  Array1.sub road.mark.buffer start len

let pack_init () =
  let road = road () in
  let words = road.heap_words in
  road.pack.bitset <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.pack.pap <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.pack.pum <- Array1.create Int32 C_layout ((words + 31) / 32);
  road.pack.buffer <- Array1.create Int32 C_layout (max default_pack_size words);
  road.pack.size <- Array1.dim road.pack.buffer;
  road.pack.len <- 0;
  Array1.fill road.pack.bitset Int32.zero;
  Array1.fill road.pack.pap Int32.zero;
  Array1.fill road.pack.pum Int32.zero

let pack_done () =
  let road = road () in
  road.pack.bitset <- empty_words ();
  road.pack.pap <- empty_words ();
  road.pack.pum <- empty_words ();
  road.pack.buffer <- empty_words ();
  road.pack.size <- 0;
  road.pack.len <- 0

let pack_alloc len =
  let road = road () in
  ensure_pack_capacity road.pack len;
  let start = road.pack.len in
  road.pack.len <- start + len;
  Array1.sub road.pack.buffer start len

let waloc len =
  let road = road () in
  let block = Array1.create Int32 C_layout len in
  Profile.record_alloc road.profile len;
  Hooks.notify_allocation road.hooks len;
  block

let wealloc old len =
  let road = road () in
  let block = Array1.create Int32 C_layout len in
  let old_len = Array1.dim old in
  let copy_len = min old_len len in
  if copy_len > 0 then
    Array1.blit (Array1.sub old 0 copy_len) (Array1.sub block 0 copy_len);
  Profile.record_realloc road.profile old_len len;
  Hooks.notify_reallocation road.hooks old_len len;
  block

let wfree block =
  let road = road () in
  let len = Array1.dim block in
  Profile.record_free road.profile len;
  Hooks.notify_free road.hooks len

let wtrim block ~old ~len =
  let road = road () in
  Profile.record_realloc road.profile old len;
  Hooks.notify_reallocation road.hooks old len;
  ignore block

let calloc num len =
  let words = ((num * len) + 3) / 4 in
  let block = waloc words in
  for i = 0 to Array1.dim block - 1 do
    Array1.set block i Int32.zero
  done;
  block

let malloc bytes =
  let words = (bytes + 3) / 4 in
  waloc words

let cell_words = 4

let celloc () = waloc cell_words

let cfree = wfree

let realloc block_opt bytes =
  match block_opt with
  | None -> malloc bytes
  | Some block ->
      let words = (bytes + 3) / 4 in
      wealloc block words

let free = function
  | None -> ()
  | Some block -> wfree block

let profile () =
  let road = road () in
  Profile.snapshot road.profile

let reset_profile () =
  let road = road () in
  Profile.reset road.profile

let register_allocation_hook hook =
  let road = road () in
  road.hooks.allocation <- hook :: road.hooks.allocation

let register_free_hook hook =
  let road = road () in
  road.hooks.free <- hook :: road.hooks.free

let register_reallocation_hook hook =
  let road = road () in
  road.hooks.reallocation <- hook :: road.hooks.reallocation

let rec register_callbacks () =
  Callback.register "u3a_ocaml_init_once" init_once;
  Callback.register "u3a_ocaml_init_heap" (fun words -> init_heap ~words () );
  Callback.register "u3a_ocaml_drop_heap" drop_heap;
  Callback.register "u3a_ocaml_mark_init" mark_init;
  Callback.register "u3a_ocaml_mark_done" mark_done;
  Callback.register "u3a_ocaml_mark_alloc" mark_alloc;
  Callback.register "u3a_ocaml_pack_init" pack_init;
  Callback.register "u3a_ocaml_pack_done" pack_done;
  Callback.register "u3a_ocaml_pack_alloc" pack_alloc;
  Callback.register "u3a_ocaml_walloc" waloc;
  Callback.register "u3a_ocaml_wealloc" wealloc;
  Callback.register "u3a_ocaml_wfree" wfree;
  Callback.register "u3a_ocaml_wtrim" wtrim;
  Callback.register "u3a_ocaml_calloc" calloc;
  Callback.register "u3a_ocaml_malloc" malloc;
  Callback.register "u3a_ocaml_celloc" celloc;
  Callback.register "u3a_ocaml_cfree" cfree;
  Callback.register "u3a_ocaml_realloc" realloc;
  Callback.register "u3a_ocaml_free" free;
  Callback.register "u3a_ocaml_register_callbacks" register_callbacks

let () = register_callbacks ()
