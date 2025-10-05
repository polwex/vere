open Bigarray

module N = Vere_runtime.Noun
module A = Vere_runtime.Allocate

let assert_bool msg cond =
  if not cond then failwith msg

let assert_int msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %d, got %d" msg expected actual)

let assert_int32 msg expected actual =
  if not (Int32.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %ld, got %ld" msg expected actual)

let assert_all_zero msg arr =
  let len = Array1.dim arr in
  let rec loop idx =
    if idx = len then ()
    else begin
      if not (Int32.equal Int32.zero (Array1.get arr idx)) then
        failwith (Printf.sprintf "%s: index %d was non-zero" msg idx);
      loop (idx + 1)
    end
  in
  loop 0

let noun_tests () =
  let cat_value = Int32.of_int 42 in
  let cat = N.Immediate cat_value in
  let cat_word = N.to_word cat in
  assert_bool "cat tag" (N.is_cat cat_word);
  (match N.of_word cat_word with
  | N.Immediate value -> assert_int32 "cat roundtrip" cat_value value
  | _ -> failwith "cat lost immediacy");
  let pug_word = N.word_of_tag N.Pug ~offset:(Int32.of_int 0x12345) in
  assert_bool "pug tag" (N.is_pug pug_word);
  assert_int "pug offset" 0x12345 (N.offset_of_word pug_word);
  (match N.of_word pug_word with
  | N.IndirectAtom { N.offset; _ } -> assert_int "pug record" 0x12345 offset
  | _ -> failwith "pug decoding failed");
  let pom_word = N.word_of_tag N.Pom ~offset:(Int32.of_int 0x222) in
  assert_bool "pom tag" (N.is_pom pom_word);
  (match N.of_word pom_word with
  | N.IndirectCell { N.offset; _ } -> assert_int "pom offset" 0x222 offset
  | _ -> failwith "pom decoding failed");
  let rich_pom =
    N.with_head_tail
      (N.of_word pom_word)
      ~head:(Some (N.Immediate (Int32.of_int 1)))
      ~tail:(Some (N.Immediate (Int32.of_int 2)))
  in
  (match rich_pom with
  | N.IndirectCell { N.head = Some head; tail = Some tail; _ } ->
      assert_bool "head immediate" (Option.is_some (N.immediate_value head));
      assert_bool "tail immediate" (Option.is_some (N.immediate_value tail))
  | _ -> failwith "pom enrichment failed");
  let rich_pug =
    N.with_payload
      (N.of_word pug_word)
      ~payload:(Some (Array1.create Int32 C_layout 1))
  in
  match rich_pug with
  | N.IndirectAtom { N.payload = Some payload; _ } ->
      assert_int "payload length" 1 (Array1.dim payload)
  | _ -> failwith "pug enrichment failed"

let allocation_tests () =
  A.init_once ();
  A.init_heap ~words:128 ();
  A.reset_profile ();
  let allocations = ref 0 in
  let frees = ref 0 in
  let reallocations = ref 0 in
  A.register_allocation_hook (fun words -> allocations := !allocations + words);
  A.register_free_hook (fun words -> frees := !frees + words);
  A.register_reallocation_hook
    (fun old_words new_words -> reallocations := !reallocations + (new_words - old_words));
  A.mark_init ();
  let mark_buf = A.mark_alloc 8 in
  assert_int "mark allocation length" 8 (Array1.dim mark_buf);
  A.pack_init ();
  let pack_buf = A.pack_alloc 4 in
  assert_int "pack allocation length" 4 (Array1.dim pack_buf);
  let block = A.waloc 16 in
  assert_int "waloc length" 16 (Array1.dim block);
  let block' = A.wealloc block 24 in
  assert_int "wealloc length" 24 (Array1.dim block');
  for i = 0 to Array1.dim block' - 1 do
    Array1.set block' i (Int32.of_int i)
  done;
  let block'' = A.wealloc block' 12 in
  assert_int "wealloc shrink" 12 (Array1.dim block'');
  for i = 0 to Array1.dim block'' - 1 do
    assert_int32 "wealloc copy" (Int32.of_int i) (Array1.get block'' i)
  done;
  let calloc_block = A.calloc 4 8 in
  assert_int "calloc length" ((4 * 8 + 3) / 4) (Array1.dim calloc_block);
  assert_all_zero "calloc zeroed" calloc_block;
  let malloc_block = A.malloc 64 in
  assert_int "malloc length" ((64 + 3) / 4) (Array1.dim malloc_block);
  ignore (A.realloc None 32);
  ignore (A.realloc (Some malloc_block) 32);
  A.wfree block'';
  A.free (Some calloc_block);
  A.free None;
  let snapshot = A.profile () in
  assert_bool "allocations tracked" (snapshot.allocations >= 3);
  assert_bool "frees tracked" (snapshot.frees >= 2);
  assert_bool "realloc tracked" (snapshot.reallocations >= 2);
  assert_bool "alloc hook" (!allocations > 0);
  assert_bool "free hook" (!frees > 0);
  assert_bool "realloc hook" (!reallocations <> 0);
  A.pack_done ();
  A.mark_done ();
  A.drop_heap ~cap:0 ~ear:0

let () =
  noun_tests ();
  allocation_tests ()
