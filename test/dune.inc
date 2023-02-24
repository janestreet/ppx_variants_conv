; GENERATED RULES

(rule
 (targets test_gadt_error.actual.ml)
 (deps
  (:pp pp.exe)
  (:input test_gadt_error.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "./%{pp} -no-color -null --impl %{input} || true"))))

(alias 
 (name runtest)
 (action
  (diff test_gadt_error.expected.ml test_gadt_error.actual.ml)))

(rule
 (targets test_polymorphic_variant_bound_error.actual.ml)
 (deps
  (:pp pp.exe)
  (:input test_polymorphic_variant_bound_error.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "./%{pp} -no-color -null --impl %{input} || true"))))

(alias 
 (name runtest)
 (action
  (diff test_polymorphic_variant_bound_error.expected.ml test_polymorphic_variant_bound_error.actual.ml)))

(rule
 (targets test_polymorphic_variant_inclusion_error.actual.ml)
 (deps
  (:pp pp.exe)
  (:input test_polymorphic_variant_inclusion_error.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "./%{pp} -no-color -null --impl %{input} || true"))))

(alias 
 (name runtest)
 (action
  (diff test_polymorphic_variant_inclusion_error.expected.ml test_polymorphic_variant_inclusion_error.actual.ml)))

(rule
 (targets test_polymorphic_variant_lower_bound_error.actual.ml)
 (deps
  (:pp pp.exe)
  (:input test_polymorphic_variant_lower_bound_error.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "./%{pp} -no-color -null --impl %{input} || true"))))

(alias 
 (name runtest)
 (action
  (diff test_polymorphic_variant_lower_bound_error.expected.ml test_polymorphic_variant_lower_bound_error.actual.ml)))

(rule
 (targets test_polymorphic_variant_upper_bound_error.actual.ml)
 (deps
  (:pp pp.exe)
  (:input test_polymorphic_variant_upper_bound_error.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "./%{pp} -no-color -null --impl %{input} || true"))))

(alias 
 (name runtest)
 (action
  (diff test_polymorphic_variant_upper_bound_error.expected.ml test_polymorphic_variant_upper_bound_error.actual.ml)))
