module Normal = struct
  type t =  Foo of int | Bar [@@deriving variants]
end

module Poly = struct
  type t =  [ `Foo of int | `Bar ] [@@deriving variants]
end
