
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

module Gadt : sig 
  type _ t = 
    | Bool : bool -> bool t
    | Int : int -> int t
    | Cond : { cond: bool t ; true_branch: 'a t ; false_branch: 'a t } -> 'a t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Swap : ('a * 'b) t -> ('b * 'a) t
    | Fst: ('a * 'b) t -> 'a t 
    | Snd : ('a * 'b) t -> 'b t
  [@@deriving variants] 
end =  struct
  type _ t = 
    | Bool : bool -> bool t
    | Int : int -> int t
    | Cond : { cond: bool t ; true_branch: 'a t ; false_branch: 'a t } -> 'a t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Swap : ('a * 'b) t -> ('b * 'a) t
    | Fst: ('a * 'b) t -> 'a t 
    | Snd : ('a * 'b) t -> 'b t
  [@@deriving variants] 
end

module Gadt_arity: sig 
  type (_,_) t = Eq: ('a,'a) t
  [@@deriving variants] 
end =  struct
  type (_,_) t = Eq: ('a,'a) t
  [@@deriving variants] 
end