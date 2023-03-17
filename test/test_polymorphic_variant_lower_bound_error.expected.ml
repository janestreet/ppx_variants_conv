[1mFile "test_polymorphic_variant_lower_bound_error.ml", line 1, characters 13-29[0m:
1 | type fail2 = [> `Foo | `Bar ] [@@deriving variants]
                 [1;31m^^^^^^^^^^^^^^^^[0m
[1;31mError[0m: ppx_variants_conv: polymorphic variants with a row variable are not supported
