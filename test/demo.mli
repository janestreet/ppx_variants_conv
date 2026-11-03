module Simple : sig
  type t =
    | Foo of int
    | Bar
    | Baz of int * string
  [@@deriving_inline variants]

  include sig
    [@@@ocaml.warning "-32-60"]

    val foo : int -> t
    val bar : t
    val baz : int -> string -> t
    val is_foo : t -> bool
    val is_bar : t -> bool
    val is_baz : t -> bool
    val foo_val : t -> int option
    val bar_val : t -> unit option
    val baz_val : t -> (int * string) option

    module Variants : sig
      val foo : (int -> t) Variantslib.Variant.t
      val bar : t Variantslib.Variant.t
      val baz : (int -> string -> t) Variantslib.Variant.t
      val to_rank : t -> int
      val to_name : t -> string
      val descriptions : (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module With_fold : sig
  type t =
    | Foo of int
    | Bar
  [@@deriving_inline variants ~fold]

  include sig
    [@@@ocaml.warning "-32-60"]

    val foo : int -> t
    val bar : t
    val is_foo : t -> bool
    val is_bar : t -> bool
    val foo_val : t -> int option
    val bar_val : t -> unit option

    module Variants : sig
      val foo : (int -> t) Variantslib.Variant.t
      val bar : t Variantslib.Variant.t

      val fold
        : 'acc__0 'acc__1 'acc__2.
        init:'acc__0
        -> foo:('acc__0 -> (int -> t) Variantslib.Variant.t -> 'acc__1)
        -> bar:('acc__1 -> t Variantslib.Variant.t -> 'acc__2)
        -> 'acc__2

      val to_rank : t -> int
      val to_name : t -> string
      val descriptions : (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val constructor_names : string list
end

module With_make_matcher : sig
  type t =
    | Foo of int
    | Bar
  [@@deriving_inline variants ~make_matcher]

  include sig
    [@@@ocaml.warning "-32-60"]

    val foo : int -> t
    val bar : t
    val is_foo : t -> bool
    val is_bar : t -> bool
    val foo_val : t -> int option
    val bar_val : t -> unit option

    module Variants : sig
      val foo : (int -> t) Variantslib.Variant.t
      val bar : t Variantslib.Variant.t

      val make_matcher
        : 'result__ 'acc__0 'acc__1 'acc__2.
        foo:((int -> t) Variantslib.Variant.t -> 'acc__0 -> (int -> 'result__) * 'acc__1)
        -> bar:(t Variantslib.Variant.t -> 'acc__1 -> (unit -> 'result__) * 'acc__2)
        -> 'acc__0
        -> (t -> 'result__) * 'acc__2

      val to_rank : t -> int
      val to_name : t -> string
      val descriptions : (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Inline_record : sig
  type t =
    | Foo of
        { a : int
        ; b : string
        }
    | Bar
  [@@deriving_inline variants]

  include sig
    [@@@ocaml.warning "-32-60"]

    val foo : a:int -> b:string -> t
    val bar : t
    val is_foo : t -> bool
    val is_bar : t -> bool
    val foo_val : t -> ([ `a of int ] * [ `b of string ]) option
    val bar_val : t -> unit option

    module Variants : sig
      val foo : (a:int -> b:string -> t) Variantslib.Variant.t
      val bar : t Variantslib.Variant.t
      val to_rank : t -> int
      val to_name : t -> string
      val descriptions : (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Parameterized : sig
  type 'a t =
    | A of 'a
    | B
  [@@deriving_inline variants]

  include sig
    [@@@ocaml.warning "-32-60"]

    val a : 'a. 'a -> 'a t
    val b : 'a. 'a t
    val is_a : 'a. 'a t -> bool
    val is_b : 'a. 'a t -> bool
    val a_val : 'a. 'a t -> 'a option
    val b_val : 'a. 'a t -> unit option

    module Variants : sig
      val a : 'a. ('a -> 'a t) Variantslib.Variant.t
      val b : 'a. 'a t Variantslib.Variant.t
      val to_rank : 'a. 'a t -> int
      val to_name : 'a. 'a t -> string
      val descriptions : 'a. (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic : sig
  type t =
    [ `Foo of int
    | `Bar
    ]
  [@@deriving_inline variants]

  include sig
    [@@@ocaml.warning "-32-60"]

    val foo : int -> t
    val bar : t
    val is_foo : t -> bool
    val is_bar : t -> bool
    val foo_val : t -> int option
    val bar_val : t -> unit option

    module Variants : sig
      val foo : (int -> t) Variantslib.Variant.t
      val bar : t Variantslib.Variant.t
      val to_rank : t -> int
      val to_name : t -> string
      val descriptions : (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic_with_make_matcher : sig
  type t =
    [ `Foo of int
    | `Bar
    ]
  [@@deriving_inline variants ~make_matcher]

  include sig
    [@@@ocaml.warning "-32-60"]

    val foo : int -> t
    val bar : t
    val is_foo : t -> bool
    val is_bar : t -> bool
    val foo_val : t -> int option
    val bar_val : t -> unit option

    module Variants : sig
      val foo : (int -> t) Variantslib.Variant.t
      val bar : t Variantslib.Variant.t

      val make_matcher
        : 'result__ 'acc__0 'acc__1 'acc__2.
        foo:((int -> t) Variantslib.Variant.t -> 'acc__0 -> (int -> 'result__) * 'acc__1)
        -> bar:(t Variantslib.Variant.t -> 'acc__1 -> (unit -> 'result__) * 'acc__2)
        -> 'acc__0
        -> (t -> 'result__) * 'acc__2

      val to_rank : t -> int
      val to_name : t -> string
      val descriptions : (string * int) list
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end
