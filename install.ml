#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_variants_conv"
  [ oasis_lib "ppx_variants_conv"
  ; file "META" ~section:"lib"
  ]
