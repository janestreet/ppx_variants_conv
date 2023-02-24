#!/bin/bash
DIRECTORY=.
out=${1:-dune.inc}
echo "; GENERATED RULES" > $out

for filename in $(ls | egrep -i 'test_([a-z0-9]+_)+error.ml'); do
    file=$(basename -- "$filename");
    pfx="${file%.*}";

    echo "
(rule
 (targets $pfx.actual.ml)
 (deps
  (:pp pp.exe)
  (:input $pfx.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash \"./%{pp} -no-color -null --impl %{input} || true\"))))

(alias 
 (name runtest)
 (action
  (diff $pfx.expected.ml $pfx.actual.ml)))" >> $out;

done