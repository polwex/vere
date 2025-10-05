open Bigarray

(** Runtime representation of urbit nouns in OCaml. Mirrors the 31-bit
    immediate atom ("cat") and indirect pug/pom encodings used by the C
    runtime. *)

type tag =
  | Cat  (** Immediate 31-bit atom. *)
  | Pug  (** Indirect atom stored off-heap. *)
  | Pom  (** Indirect cell stored off-heap. *)

(** A pointer offset encoded in the low 30 bits of a noun word. *)
type offset = int

(** Indirect atom metadata. *)
type pug = {
  offset : offset;  (** Loom-style offset encoded in the noun tag. *)
  length : int option;  (** Optional length in 32-bit words. *)
  payload : (int32, int32_elt, c_layout) Array1.t option;
      (** Optional backing store for OCaml-managed atoms. *)
}

(** Indirect cell metadata. *)
type pom = {
  offset : offset;  (** Loom-style offset encoded in the noun tag. *)
  head : t option;  (** Optional OCaml shadow of the head noun. *)
  tail : t option;  (** Optional OCaml shadow of the tail noun. *)
}

and t =
  | Immediate of int32
  | IndirectAtom of pug
  | IndirectCell of pom

let tag_mask = Int32.of_int 0xC0000000
let cat_mask = Int32.of_int 0x80000000
let pom_mask = Int32.of_int 0xC0000000
let offset_mask = Int32.of_int 0x3FFFFFFF
let cat_payload_mask = Int32.of_int 0x7FFFFFFF

let low30 word = Int32.(to_int (logand word offset_mask))

let sanitize_offset offset = Int32.logand offset offset_mask

let tag_of_word word =
  match Int32.logand word tag_mask with
  | x when Int32.equal x Int32.zero -> Cat
  | x when Int32.equal x cat_mask -> Pug
  | x when Int32.equal x pom_mask -> Pom
  | _ -> Pom (* should be unreachable, but treat as cell *)

let offset_of_word word = low30 word

let word_of_tag tag ~offset =
  let off = sanitize_offset offset in
  match tag with
  | Cat -> Int32.logand off cat_payload_mask
  | Pug -> Int32.logor cat_mask off
  | Pom -> Int32.logor pom_mask off

let is_cat word =
  Int32.equal (Int32.logand word cat_mask) Int32.zero

let is_pug word =
  Int32.equal (Int32.logand word tag_mask) cat_mask

let is_pom word =
  Int32.equal (Int32.logand word tag_mask) pom_mask

let to_word = function
  | Immediate value ->
      let payload = Int32.logand value cat_payload_mask in
      word_of_tag Cat ~offset:payload
  | IndirectAtom { offset; _ } -> word_of_tag Pug ~offset:(Int32.of_int offset)
  | IndirectCell { offset; _ } -> word_of_tag Pom ~offset:(Int32.of_int offset)

let of_word word =
  match tag_of_word word with
  | Cat -> Immediate (Int32.logand word cat_payload_mask)
  | Pug ->
      IndirectAtom
        { offset = offset_of_word word; length = None; payload = None }
  | Pom ->
      IndirectCell { offset = offset_of_word word; head = None; tail = None }

let immediate_value = function
  | Immediate value -> Some value
  | IndirectAtom _ | IndirectCell _ -> None

let with_head_tail noun ~head ~tail =
  match noun with
  | IndirectCell cell -> IndirectCell { cell with head; tail }
  | _ -> noun

let with_payload noun ~payload =
  match noun with
  | IndirectAtom atom -> IndirectAtom { atom with payload }
  | _ -> noun
