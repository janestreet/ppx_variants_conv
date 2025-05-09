(** Alpha-renames type variables in the same type environment. Also replaces wildcard type
    variables ([_]) with new names *)

open Base
open Ppxlib

(** Stores the mapping from old to new type variable names *)
type t

val empty : t

(** Rename type variables while updating [t] *)
val fold_map : t Ast_traverse.fold_map

(** Rename a single type variable name. If [t] has already renamed a type variable with
    that name, it returns the same new name, otherwise, it generates a fresh name *)
val var : t -> string -> t * string

(** The range of the "old-to-new-names" map, i.e., all fresh names that [t] generated *)
val range : t -> string list
