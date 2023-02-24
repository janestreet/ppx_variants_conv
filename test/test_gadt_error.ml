module Gadt : sig 
  type _ t = 
    | Bool : bool -> bool t
    | Int : int -> int t
    | Cond : bool t * 'a t * 'a t -> 'a t
  [@@deriving variants] 
end =  struct
  type _ t = 
    | Bool : bool -> bool t
    | Int : int -> int t
    | Cond : bool t * 'a t * 'a t -> 'a t
  [@@deriving variants] 
end