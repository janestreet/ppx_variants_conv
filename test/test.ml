
module Normal : sig 
  type t = Foo of int | Bar | Exception [@@deriving variants]
end = struct
  type t = Foo of int | Bar | Exception [@@deriving variants]
end

module Normal_inline_record : sig 
  type t =  Foo of { a : int; b : string} | Bar [@@deriving variants]
end = struct
  type t =  Foo of { a : int; b : string} | Bar [@@deriving variants]
end

module Poly : sig
  type t =  [ `Foo of int | `Bar | `Exception ] [@@deriving variants]
end= struct
  type t =  [ `Foo of int | `Bar | `Exception ] [@@deriving variants]
end

module Wildcard : sig
  type _ t = A | B [@@deriving variants]
end = struct
  type _ t = A | B [@@deriving variants]
end