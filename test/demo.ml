module Simple = struct
  type t =
    | Foo of int
    | Bar
    | Baz of int * string
  [@@deriving_inline variants]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let foo v0 = Foo v0
    let _ = foo
    let bar = Bar
    let _ = bar
    let baz v0 v1 = Baz (v0, v1)
    let _ = baz

    let is_foo = function
      | Foo _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_foo

    let is_bar = function
      | Bar -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_bar

    let is_baz = function
      | Baz _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_baz

    let foo_val = function
      | Foo v0 -> Stdlib.Option.Some v0
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = foo_val

    let bar_val = function
      | Bar -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = bar_val

    let baz_val = function
      | Baz (v0, v1) -> Stdlib.Option.Some (v0, v1)
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = baz_val

    module Variants = struct
      let foo = { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo }
      let _ = foo
      let bar = { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar }
      let _ = bar
      let baz = { Variantslib.Variant.name = "Baz"; rank = 2; constructor = baz }
      let _ = baz

      let to_rank : _ -> _ = function
        | Foo _ -> 0
        | Bar -> 1
        | Baz _ -> 2
      ;;

      let _ = to_rank

      let to_name = function
        | Foo _ -> "Foo"
        | Bar -> "Bar"
        | Baz _ -> "Baz"
      ;;

      let _ = to_name
      let descriptions = [ "Foo", 1; "Bar", 0; "Baz", 2 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module With_fold = struct
  type t =
    | Foo of int
    | Bar
  [@@deriving_inline variants ~fold]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let foo v0 = Foo v0
    let _ = foo
    let bar = Bar
    let _ = bar

    let is_foo = function
      | Foo _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_foo

    let is_bar = function
      | Bar -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_bar

    let foo_val = function
      | Foo v0 -> Stdlib.Option.Some v0
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = foo_val

    let bar_val = function
      | Bar -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = bar_val

    module Variants = struct
      let foo = { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo }
      let _ = foo
      let bar = { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar }
      let _ = bar

      let fold ~init:init__ ~foo:foo_fun__ ~bar:bar_fun__ =
        bar_fun__ (foo_fun__ init__ foo) bar
      ;;

      let _ = fold

      let to_rank : _ -> _ = function
        | Foo _ -> 0
        | Bar -> 1
      ;;

      let _ = to_rank

      let to_name = function
        | Foo _ -> "Foo"
        | Bar -> "Bar"
      ;;

      let _ = to_name
      let descriptions = [ "Foo", 1; "Bar", 0 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]

  let constructor_names =
    let add names variant = variant.Variantslib.Variant.name :: names in
    Variants.fold ~init:[] ~foo:add ~bar:add |> List.rev
  ;;
end

module With_make_matcher = struct
  type t =
    | Foo of int
    | Bar
  [@@deriving_inline variants ~make_matcher]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let foo v0 = Foo v0
    let _ = foo
    let bar = Bar
    let _ = bar

    let is_foo = function
      | Foo _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_foo

    let is_bar = function
      | Bar -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_bar

    let foo_val = function
      | Foo v0 -> Stdlib.Option.Some v0
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = foo_val

    let bar_val = function
      | Bar -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = bar_val

    module Variants = struct
      let foo = { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo }
      let _ = foo
      let bar = { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar }
      let _ = bar

      let make_matcher ~foo:foo_fun__ ~bar:bar_fun__ compile_acc__ =
        let foo_gen__, compile_acc__ = foo_fun__ foo compile_acc__ in
        let bar_gen__, compile_acc__ = bar_fun__ bar compile_acc__ in
        ( (function
            | Foo v0 -> foo_gen__ v0
            | Bar -> bar_gen__ ())
        , compile_acc__ )
      ;;

      let _ = make_matcher

      let to_rank : _ -> _ = function
        | Foo _ -> 0
        | Bar -> 1
      ;;

      let _ = to_rank

      let to_name = function
        | Foo _ -> "Foo"
        | Bar -> "Bar"
      ;;

      let _ = to_name
      let descriptions = [ "Foo", 1; "Bar", 0 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module Inline_record = struct
  type t =
    | Foo of
        { a : int
        ; b : string
        }
    | Bar
  [@@deriving_inline variants]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let foo ~a:v0 ~b:v1 = Foo { a = v0; b = v1 }
    let _ = foo
    let bar = Bar
    let _ = bar

    let is_foo = function
      | Foo _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_foo

    let is_bar = function
      | Bar -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_bar

    let foo_val = function
      | Foo { a = v0; b = v1 } -> Stdlib.Option.Some (`a v0, `b v1)
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = foo_val

    let bar_val = function
      | Bar -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = bar_val

    module Variants = struct
      let foo = { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo }
      let _ = foo
      let bar = { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar }
      let _ = bar

      let to_rank : _ -> _ = function
        | Foo _ -> 0
        | Bar -> 1
      ;;

      let _ = to_rank

      let to_name = function
        | Foo _ -> "Foo"
        | Bar -> "Bar"
      ;;

      let _ = to_name
      let descriptions = [ "Foo", 2; "Bar", 0 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module Parameterized = struct
  type 'a t =
    | A of 'a
    | B
  [@@deriving_inline variants]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : 'a t) -> ()
    let a v0 = A v0
    let _ = a
    let b = B
    let _ = b

    let is_a = function
      | A _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_a

    let is_b = function
      | B -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_b

    let a_val = function
      | A v0 -> Stdlib.Option.Some v0
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = a_val

    let b_val = function
      | B -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = b_val

    module Variants = struct
      let a = { Variantslib.Variant.name = "A"; rank = 0; constructor = a }
      let _ = a
      let b = { Variantslib.Variant.name = "B"; rank = 1; constructor = b }
      let _ = b

      let to_rank : _ -> _ = function
        | A _ -> 0
        | B -> 1
      ;;

      let _ = to_rank

      let to_name = function
        | A _ -> "A"
        | B -> "B"
      ;;

      let _ = to_name
      let descriptions = [ "A", 1; "B", 0 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic = struct
  type t =
    [ `Foo of int
    | `Bar
    ]
  [@@deriving_inline variants]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let foo v0 = `Foo v0
    let _ = foo
    let bar = `Bar
    let _ = bar

    let is_foo = function
      | `Foo _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_foo

    let is_bar = function
      | `Bar -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_bar

    let foo_val = function
      | `Foo v -> Stdlib.Option.Some v
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = foo_val

    let bar_val = function
      | `Bar -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = bar_val

    module Variants = struct
      let foo = { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo }
      let _ = foo
      let bar = { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar }
      let _ = bar

      let to_rank : _ -> _ = function
        | `Foo _ -> 0
        | `Bar -> 1
      ;;

      let _ = to_rank

      let to_name = function
        | `Foo _ -> "Foo"
        | `Bar -> "Bar"
      ;;

      let _ = to_name
      let descriptions = [ "Foo", 1; "Bar", 0 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic_with_make_matcher = struct
  type t =
    [ `Foo of int
    | `Bar
    ]
  [@@deriving_inline variants ~make_matcher]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let foo v0 = `Foo v0
    let _ = foo
    let bar = `Bar
    let _ = bar

    let is_foo = function
      | `Foo _ -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_foo

    let is_bar = function
      | `Bar -> true
      | _ -> false
    [@@warning "-4"]
    ;;

    let _ = is_bar

    let foo_val = function
      | `Foo v -> Stdlib.Option.Some v
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = foo_val

    let bar_val = function
      | `Bar -> Stdlib.Option.Some ()
      | _ -> Stdlib.Option.None
    [@@warning "-4"]
    ;;

    let _ = bar_val

    module Variants = struct
      let foo = { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo }
      let _ = foo
      let bar = { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar }
      let _ = bar

      let make_matcher ~foo:foo_fun__ ~bar:bar_fun__ compile_acc__ =
        let foo_gen__, compile_acc__ = foo_fun__ foo compile_acc__ in
        let bar_gen__, compile_acc__ = bar_fun__ bar compile_acc__ in
        ( (function
            | `Foo v0 -> foo_gen__ v0
            | `Bar -> bar_gen__ ())
        , compile_acc__ )
      ;;

      let _ = make_matcher

      let to_rank : _ -> _ = function
        | `Foo _ -> 0
        | `Bar -> 1
      ;;

      let _ = to_rank

      let to_name = function
        | `Foo _ -> "Foo"
        | `Bar -> "Bar"
      ;;

      let _ = to_name
      let descriptions = [ "Foo", 1; "Bar", 0 ]
      let _ = descriptions
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end
