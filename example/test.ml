module Normal = struct
  type t =
    | Foo of int
    | Bar
  [@@deriving variants]
end

module Map = struct
  type t =
    | Foo
    | Bar of int
  [@@deriving variants ~map]

  let name = Variants.map ~foo:(fun _ -> "foo") ~bar:(fun _ _ -> "bar")
end

module Iter = struct
  type t =
    | Foo
    | Bar
  [@@deriving variants ~iter]

  let names () =
    let names = ref [] in
    Variants.iter
      ~foo:(fun variant -> names := variant.name :: !names)
      ~bar:(fun variant -> names := variant.name :: !names);
    List.rev !names
  ;;
end

module Normal_inline_record = struct
  type t =
    | Foo of
        { a : int
        ; b : string
        }
    | Bar
  [@@deriving variants]
end

module Poly = struct
  type t =
    [ `Foo of int
    | `Bar
    ]
  [@@deriving variants]
end

module Unboxed = struct
  type t =
    | Foo
    | Bar of
        { x : float#
        ; y : float#
        } [@variants.non_value]
  [@@deriving variants]
end
