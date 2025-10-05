(** Runtime representation of urbit nouns in OCaml. *)

open Bigarray

(** Bit-level tags that discriminate noun storage variants. *)
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

val tag_of_word : int32 -> tag
(** [tag_of_word word] inspects the two high bits of [word] to recover the
    noun tag. *)

val offset_of_word : int32 -> offset
(** Extract the 30-bit offset component of a noun word. *)

val word_of_tag : tag -> offset:int32 -> int32
(** [word_of_tag tag ~offset] constructs a noun word with the given tag and
    offset payload. The [offset] argument is masked down to 30 bits. *)

val is_cat : int32 -> bool
val is_pug : int32 -> bool
val is_pom : int32 -> bool

val to_word : t -> int32
(** Convert an OCaml noun descriptor back into a tagged noun word. *)

val of_word : int32 -> t
(** [of_word word] creates an OCaml noun descriptor that mirrors the tag and
    offset encoded in [word]. Indirect payloads are initially opaque; callers
    can attach OCaml-managed content by updating the returned record. *)

val immediate_value : t -> int32 option
(** Attempt to recover the immediate atomic value represented by a noun. *)

val with_head_tail : t -> head:t option -> tail:t option -> t
(** [with_head_tail noun ~head ~tail] enriches an indirect cell with OCaml
    managed head and tail values. For non-cell nouns this is a no-op. *)

val with_payload : t -> payload:(int32, int32_elt, c_layout) Array1.t option -> t
(** Attach OCaml-managed storage to an indirect atom. *)
