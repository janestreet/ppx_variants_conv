[1mFile "test_polymorphic_variant_inclusion_error.ml", line 1, characters 0-53[0m:
1 | type fail1 = [ Poly.t | `Blah ] [@@deriving variants]
    [1;31m^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^[0m
[1;31mError[0m: ppx_variants_conv: polymorphic variant inclusion is not supported
