module Normal = struct
  type t =
    | Foo of int
    | Bar
  [@@deriving variants]
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
        { x : float
        ; y : float
        } [@variants.non_value]
  [@@deriving variants]
end
