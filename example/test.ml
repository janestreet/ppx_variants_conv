type t =  Foo of int | Bar [@@deriving variants]
(*let print_all_variants =
  List.map print_endline (List.map fst Variants.descriptions)*)
