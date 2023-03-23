ppx_variants_conv
=================

Generation of accessor and iteration functions for ocaml variant types.

`ppx_variants_conv` is a ppx rewriter that can be used to define first
class values representing variant constructors, some helper functions
to identify or match on individual constructors, and additional
routines to fold, iterate and map over all constructors of a variant
type.

It provides corresponding functionality for variant types as
`ppx_fields_conv` provides for record types.

# Basic use of `[@@deriving variants]` and variantslib

This code:

```ocaml
type 'a t =
  | A of 'a
  | B of char
  | C
  | D of int * int
  [@@deriving variants]
```

generates the following values:

```ocaml
(** first-class constructor functions *)
val a : 'a -> 'a t
val b : char -> 'a t
val c : 'a t
val d : int -> int -> 'a t

val is_a : _ t -> bool
val is_b : _ t -> bool
val is_c : _ t -> bool
val is_d : _ t -> bool

val a_val : 'a t -> 'a option
val b_val : _ t -> char option
val c_val : _ t -> unit option
val d_val : _ t -> (int * int) option

(** higher order variants and functions over all variants *)
module Variants : sig
  val a : ('a -> 'a t)         Variant.t
  val b : (char -> 'a t)       Variant.t
  val c : ('a t)               Variant.t
  val d : (int -> int -> 'a t) Variant.t

  val fold :
    init: 'acc1
    -> a:('acc1 -> ('a -> 'a t)         Variant.t -> 'acc2)
    -> b:('acc2 -> (char -> 'a t)       Variant.t -> 'acc3)
    -> c:('acc3 -> ('a t)               Variant.t -> 'acc4)
    -> d:('acc4 -> (int -> int -> 'a t) Variant.t -> 'acc5)
    -> 'acc5

  val iter :
       a: (('a -> 'a t)         Variant.t -> unit)
    -> b: ((char -> 'a t)       Variant.t -> unit)
    -> c: (('a t)               Variant.t -> unit)
    -> d: ((int -> int -> 'a t) Variant.t -> unit)
    -> unit

  val map :
    'a t
    -> a: (('a -> 'a t)         Variant.t -> 'a                 -> 'r)
    -> b: ((char -> 'a t)       Variant.t -> char               -> 'r)
    -> c: (('a t)               Variant.t                       -> 'r)
    -> d: ((int -> int -> 'a t) Variant.t -> int -> int -> 'a t -> 'r)
    -> 'r

  val make_matcher :
       a:(('b -> 'b t)        Variant.t -> 'acc1 -> ('a -> 'r)         * 'acc2)
    -> b:((char -> _ t)       Variant.t -> 'acc2 -> (char -> 'r)       * 'acc3)
    -> c:(_ t                 Variant.t -> 'acc3 -> (unit -> 'r)       * 'acc4)
    -> d:((int -> int -> _ t) Variant.t -> 'acc4 -> (int -> int -> 'r) * 'acc5)
    -> 'acc1
    -> ('a t -> 'r) * 'acc5

  val to_rank : _ t -> int
  val to_name : _ t -> string

  (** name * number of arguments, ie [("A", 1); ("B", 1); ("C", 0); ("D", 2)]. *)
  val descriptions : (string * int) list
end
```

Variant.t is defined in Variantslib as follows:

```ocaml
module Variant = struct
  type 'constructor t = {
    name : string;
    (* the position of the constructor in the type definition, starting from 0 *)
    rank : int;
    constructor : 'constructor
  }
end
```

The fold, iter, and map functions are useful in dealing with the totality of variants.
For example, to get a list of all variants when all the constructors are nullary:

```ocaml
type t =
  | First
  | Second
  | Third
  [@@deriving variants]
```

```ocaml
let all =
  let add acc var = var.Variantslib.Variant.constructor :: acc in
  Variants.fold ~init:[]
    ~first:add
    ~second:add
    ~third:add
```

Just like with `[@@deriving fields]`, if the type changes, the
compiler will complain until this definition is updated as well.

# Polymorphic Variants

`ppx_variants_conv` works similarly on simple polymorphic variants
(without row variables and without inclusion).

# GADTs

`ppx_variants_conv` can be used with GADTs but with some limitations. 

The preprocessor will not generate `*_val` functions for the GADTs
constructors, because it's not clear which type such functions should
have. This may be revisited in the future. As a consequence neither
`Variants.map` nor `Variants.make_matcher` functions will be generated
either.
