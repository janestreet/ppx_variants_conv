(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)

open Base
open Ppxlib
open Ast_builder.Default

let raise_unsupported loc =
  Location.raise_errorf
    ~loc
    "Unsupported use of variants (you can only use it on variant types)."
;;

module Create = struct
  let lambda loc xs body =
    List.fold_right xs ~init:body ~f:(fun (label, p) e -> pexp_fun ~loc label None p e)
  ;;

  let lambda_sig loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun (label, arg_ty) acc ->
      ptyp_arrow ~loc label arg_ty acc)
  ;;
end

module Variant_constructor = struct
  type t =
    { name : string
    ; loc : Location.t
    ; has_non_value_flag : bool
    ; kind :
        [ `Normal of core_type list * core_type option
        | `Normal_inline_record of label_declaration list * core_type option
        | `Polymorphic of core_type option
        ]
    ; vars : core_type list
    (* variables in a gadt constructor. See comment above [alpha_rename_if_gadt] *)
    }

  let args t =
    match t.kind with
    | `Normal (pcd_args, _) ->
      List.mapi pcd_args ~f:(fun i _ -> Nolabel, "v" ^ Int.to_string i)
    | `Normal_inline_record (fields, _) ->
      List.mapi fields ~f:(fun i f -> Labelled f.pld_name.txt, "v" ^ Int.to_string i)
    | `Polymorphic None -> []
    | `Polymorphic (Some _) -> [ Nolabel, "v0" ]
  ;;

  let return_ty_opt t =
    match t.kind with
    | `Normal (_, return_ty_opt) | `Normal_inline_record (_, return_ty_opt) ->
      return_ty_opt
    | `Polymorphic _ -> None
  ;;

  let return_ty t ~variant_type = Option.value (return_ty_opt t) ~default:variant_type

  (* We could be smarter and avoid treating as GADT constructors that use the GADT syntax
     but not the expressivity of GADTs. It's not clearly useful though. *)
  let is_gadt t = Option.is_some (return_ty_opt t)

  let pattern_without_binding { name; loc; kind; has_non_value_flag = _; vars = _ } =
    match kind with
    | `Normal ([], _) -> ppat_construct ~loc (Located.lident ~loc name) None
    | `Normal (_ :: _, _) | `Normal_inline_record _ ->
      ppat_construct ~loc (Located.lident ~loc name) (Some (ppat_any ~loc))
    | `Polymorphic None -> ppat_variant ~loc name None
    | `Polymorphic (Some _) -> ppat_variant ~loc name (Some (ppat_any ~loc))
  ;;

  let to_fun_type t ~rhs:body_ty =
    let arg_types =
      match t.kind with
      | `Polymorphic None -> []
      | `Polymorphic (Some v) -> [ Nolabel, v ]
      | `Normal (args, _) -> List.map args ~f:(fun typ -> Nolabel, typ)
      | `Normal_inline_record (fields, _) ->
        List.map fields ~f:(fun cd -> Labelled cd.pld_name.txt, cd.pld_type)
    in
    Create.lambda_sig t.loc arg_types body_ty
  ;;

  let has_getter t =
    (* We cannot generate a getter for GADTs in the case that we have existentially
       quantified type vars. Further we can't generate _regular_ getter functions
       when the GADT is non-regular. For the time being we skip generating getters
       for GADTs altogether.

       We also do not currently support generating getters for types with non-value
       layouts. *)
    not (t.has_non_value_flag || is_gadt t)
  ;;

  let to_getter_type t ~lhs:input_type =
    match has_getter t with
    | false -> None
    | true ->
      let variant_for_label (ld : label_declaration) =
        ptyp_variant
          ~loc:ld.pld_loc
          [ rtag ~loc:ld.pld_loc ld.pld_name false [ ld.pld_type ] ]
          Closed
          None
      in
      let loc = t.loc in
      let result_type =
        match t.kind with
        | `Polymorphic None | `Normal ([], _) | `Normal_inline_record ([], _) ->
          [%type: unit]
        | `Polymorphic (Some v) -> v
        | `Normal (tup, _) -> ptyp_tuple ~loc tup
        | `Normal_inline_record (fields, _) ->
          ptyp_tuple ~loc (List.map fields ~f:variant_for_label)
      in
      Some (ptyp_arrow ~loc Nolabel input_type [%type: [%t result_type] option])
  ;;

  let to_getter_case { loc; name; kind; has_non_value_flag = _; vars = _ } =
    let pat, idents =
      match kind with
      | `Polymorphic None -> ppat_variant ~loc name None, []
      | `Polymorphic (Some _) ->
        let ident = "v" in
        ppat_variant ~loc name (Some (pvar ident ~loc)), [ `Unlabelled ident ]
      | `Normal ([], _) -> ppat_construct ~loc (Located.lident ~loc name) None, []
      | `Normal (args, _) ->
        let idents = List.mapi args ~f:(fun i _ -> "v" ^ Int.to_string i) in
        let patterns = List.map idents ~f:(pvar ~loc) in
        ( ppat_construct ~loc (Located.lident ~loc name) (Some (ppat_tuple ~loc patterns))
        , List.map idents ~f:(fun i -> `Unlabelled i) )
      | `Normal_inline_record (lds, _) ->
        let patterns, idents =
          List.mapi lds ~f:(fun i ld ->
            let ident = "v" ^ Int.to_string i in
            let field_pat = Located.map_lident ld.pld_name, pvar ident ~loc in
            field_pat, `Labelled (ld.pld_name.txt, ident))
          |> List.unzip
        in
        ( ppat_construct
            ~loc
            (Located.lident ~loc name)
            (Some (ppat_record ~loc patterns Closed))
        , idents )
    in
    let ident_expr = function
      | `Unlabelled ident -> evar ~loc ident
      | `Labelled (label, ident) -> pexp_variant ~loc label (Some (evar ~loc ident))
    in
    let expr =
      match idents with
      | [] -> [%expr ()]
      | idents -> pexp_tuple ~loc (List.map idents ~f:ident_expr)
    in
    pat, expr
  ;;
end

let variant_name_to_string v =
  let s = String.lowercase v in
  if Keyword.is_keyword s then s ^ "_" else s
;;

module Inspect = struct
  let non_value_row_field = Attribute.declare_flag "variants.non_value" Rtag

  let non_value_constructor_declaration =
    Attribute.declare_flag "variants.non_value" Constructor_declaration
  ;;

  let row_field loc rf : Variant_constructor.t =
    let has_non_value_flag = Attribute.has_flag non_value_row_field rf in
    match rf.prf_desc with
    | Rtag ({ txt = name; _ }, true, _) | Rtag ({ txt = name; _ }, _, []) ->
      { name; loc; has_non_value_flag; kind = `Polymorphic None; vars = [] }
    | Rtag ({ txt = name; _ }, false, tp :: _) ->
      { name; loc; has_non_value_flag; kind = `Polymorphic (Some tp); vars = [] }
    | Rinherit _ ->
      Location.raise_errorf
        ~loc
        "ppx_variants_conv: polymorphic variant inclusion is not supported"
  ;;

  (* Constructors that use GADT syntax have their own namespace of type variables. We
     alpha-rename because we combine these types for the signatures of derived functions
     like [fold] and [iter].

     For example, in [type _ t = F64 : ('a : float64) . 'a t | V : 'a t], there are two
     distinct type variables called ['a]. For the first constructor, this function returns
     [F64 : ('fresh1 : float64) . 'fresh1 t] and [[("fresh1", Some float64)]]. For the
     second, this returns [V : 'fresh2 t] and [[("fresh2", None)]].
  *)
  let alpha_rename_if_gadt cd =
    if Option.is_some cd.pcd_res
    then (
      let cd, renaming =
        Alpha_renaming.fold_map#constructor_declaration cd Alpha_renaming.empty
      in
      let pcd_vars =
        Ppxlib_jane.Shim.Constructor_declaration.extract_vars_with_jkind_annotations cd
      in
      let bound_vars = List.map pcd_vars ~f:(fun (s, jkind) -> s.txt, jkind) in
      let bound_vars_names = List.map ~f:fst bound_vars |> Set.of_list (module String) in
      (* All free vars not bound get jkind [None] *)
      let free_vars_names =
        Alpha_renaming.range renaming
        |> List.filter ~f:(fun s -> not (Set.mem bound_vars_names s))
      in
      cd, bound_vars @ List.map ~f:(fun s -> s, None) free_vars_names)
    else cd, []
  ;;

  let constructor cd : Variant_constructor.t =
    let has_non_value_flag = Attribute.has_flag non_value_constructor_declaration cd in
    let cd, vars = alpha_rename_if_gadt cd in
    let vars =
      List.map vars ~f:(fun (s, jkind) ->
        Ppxlib_jane.Ast_builder.Default.Latest.ptyp_var s jkind ~loc:cd.pcd_loc)
    in
    let kind =
      match cd.pcd_args with
      | Pcstr_tuple pcd_args ->
        let core_types =
          List.map pcd_args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type
        in
        `Normal (core_types, cd.pcd_res)
      | Pcstr_record fields -> `Normal_inline_record (fields, cd.pcd_res)
    in
    { name = cd.pcd_name.txt; loc = cd.pcd_name.loc; has_non_value_flag; kind; vars }
  ;;

  let type_decl td =
    let loc = td.ptype_loc in
    match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
    | Ptype_variant cds ->
      let cds = List.map cds ~f:constructor in
      let names_as_string = Hashtbl.create (module String) in
      List.iter cds ~f:(fun { name; loc; _ } ->
        let s = variant_name_to_string name in
        match Hashtbl.find names_as_string s with
        | None -> Hashtbl.add_exn names_as_string ~key:s ~data:name
        | Some name' ->
          Location.raise_errorf
            ~loc
            "ppx_variants_conv: constructors %S and %S both get mapped to value %S"
            name
            name'
            s);
      cds
    | Ptype_record _ | Ptype_record_unboxed_product _ | Ptype_open ->
      raise_unsupported loc
    | Ptype_abstract ->
      (match td.ptype_manifest with
       | Some { ptyp_desc = Ptyp_variant (row_fields, Closed, None); _ } ->
         List.map row_fields ~f:(row_field loc)
       | Some { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
         Location.raise_errorf
           ~loc
           "ppx_variants_conv: polymorphic variants with a row variable are not supported"
       | _ -> raise_unsupported loc)
  ;;
end

let variants_module = function
  | "t" -> "Variants"
  | type_name -> "Variants_of_" ^ type_name
;;

(* Extract name and jkind from type parameters, generating names for [_] parameters *)
let name_params params =
  List.map params ~f:(fun (ty, _) ->
    let loc = ty.ptyp_loc in
    let ptyp_desc = Ppxlib_jane.Shim.Core_type_desc.of_parsetree ty.ptyp_desc in
    match ptyp_desc with
    | Ptyp_var (name, jkind) -> { loc; txt = name }, jkind
    | Ptyp_any jkind -> { loc; txt = gen_symbol ~prefix:"param" () }, jkind
    | _ ->
      Location.raise_errorf
        ~loc
        "Error in ppx_variants_conv: expected either a wildcard (e.g. _) or variable \
         (e.g. 'a), but found a different type parameter construct: %s"
        (Ppxlib_jane.Language_feature_name.of_core_type_desc ptyp_desc))
;;

module Gen_sig = struct
  let label_arg _loc name ty = Asttypes.Labelled (variant_name_to_string name), ty

  let val_ ~loc ~univars name type_ =
    let type_ =
      match univars with
      | [] -> type_
      | _ ->
        let univars =
          List.map univars ~f:Ppxlib_jane.get_type_param_name_and_jkind_of_core_type
        in
        Ppxlib_jane.Ast_builder.Default.ptyp_poly ~loc univars type_
    in
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc name) ~type_ ~prim:[])
  ;;

  let variant_arg f ~variant_type (v : Variant_constructor.t) =
    let loc = v.loc in
    let return_ty = Variant_constructor.return_ty v ~variant_type in
    let variant =
      [%type: [%t Variant_constructor.to_fun_type v ~rhs:return_ty] Variantslib.Variant.t]
    in
    label_arg loc v.Variant_constructor.name (f ~variant)
  ;;

  let v_fold_fun ~variant_type ~params loc variants =
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let f i v =
      variant_arg
        ~variant_type
        (fun ~variant -> [%type: [%t acc i] -> [%t variant] -> [%t acc (i + 1)]])
        v
    in
    let types = List.mapi variants ~f in
    let init_ty = label_arg loc "init" (acc 0) in
    let t = Create.lambda_sig loc (init_ty :: types) (acc (List.length variants)) in
    let vars_in_gadt_cases = List.concat_map variants ~f:(fun v -> v.vars) in
    (* Universally quantifying all type variables allows us to give jkind annotations to
       type variables. There are a few sources of type variables here.

        1. Non-gadt constructors share the type variables of the type constructor, [params].
        2. GADT constuctors each have their own scope, whose variables are alpha renamed
            then stored in each [Variant_constructor.t]'s [vars] field.
        3. We use [acc] to create new variables.

       For example, consider [type 'a t = A of 'a | B : 'a -> 'a t].

       [A]'s ['a] is the same as the type parameter, but [B]'s ['a] is actually in its own
       scope (and gets alpha-renamed in [alpha_rename_if_gadt]). Thus, we generate the
       following.

       {[
         val fold
           : 'acc__0 'acc__1 'acc__2 'a 'a__001_.
           init:'acc__0
           -> a:('acc__0 -> ('a -> 'a t) Variantslib.Variant.t -> 'acc__1)
           -> b:('acc__1 -> ('a__001_ -> 'a__001_ t) Variantslib.Variant.t -> 'acc__2)
           -> 'acc__2
       ]}

       ['a] comes from [params] and ['a__001_] comes from [vars_in_gadt_cases].
    *)
    let univars =
      List.init (List.length variants + 1) ~f:acc @ params @ vars_in_gadt_cases
    in
    val_ ~loc ~univars "fold" t
  ;;

  let v_iter_fun ~variant_type ~params loc variants =
    let f = variant_arg ~variant_type (fun ~variant -> [%type: [%t variant] -> unit]) in
    let types = List.map variants ~f in
    (* See note in [v_fold_fun] *)
    let vars_in_gadt_cases = List.concat_map variants ~f:(fun v -> v.vars) in
    let univars = params @ vars_in_gadt_cases in
    let t = Create.lambda_sig loc types [%type: unit] in
    val_ ~loc ~univars "iter" t
  ;;

  let v_map_fun_opt ~variant_type ~params loc variants =
    let module V = Variant_constructor in
    if List.exists variants ~f:V.is_gadt
    then None
    else (
      let result_type = [%type: 'result__] in
      let f v =
        let variant =
          let constructor_type = V.to_fun_type v ~rhs:variant_type in
          Create.lambda_sig
            loc
            [ Nolabel, [%type: [%t constructor_type] Variantslib.Variant.t] ]
            (V.to_fun_type v ~rhs:result_type)
        in
        label_arg loc v.V.name variant
      in
      let types = List.map variants ~f in
      let t = Create.lambda_sig loc ((Nolabel, variant_type) :: types) result_type in
      let univars = [%type: 'result__] :: params in
      Some (val_ ~loc ~univars "map" t))
  ;;

  let v_make_matcher_fun_opt ~variant_type ~params loc variants =
    let module V = Variant_constructor in
    if List.exists variants ~f:V.is_gadt
    then None
    else (
      let result_type = [%type: 'result__] in
      let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
      let f i v =
        let variant =
          [%type:
            [%t Variant_constructor.to_fun_type v ~rhs:variant_type] Variantslib.Variant.t]
        in
        let fun_type =
          match Variant_constructor.args v with
          | [] -> [%type: unit -> [%t result_type]]
          | _ :: _ -> Variant_constructor.to_fun_type v ~rhs:result_type
        in
        label_arg
          loc
          v.name
          [%type: [%t variant] -> [%t acc i] -> [%t fun_type] * [%t acc (i + 1)]]
      in
      let types = List.mapi variants ~f in
      let t =
        Create.lambda_sig
          loc
          (types @ [ Nolabel, acc 0 ])
          [%type:
            ([%t variant_type] -> [%t result_type]) * [%t acc (List.length variants)]]
      in
      let univars =
        ([%type: 'result__] :: List.init (List.length variants + 1) ~f:acc) @ params
      in
      Some (val_ ~loc ~univars "make_matcher" t))
  ;;

  let v_descriptions ~variant_type:_ loc _ =
    val_ ~loc "descriptions" [%type: (string * int) list]
  ;;

  let v_to_rank_fun ~variant_type loc _ =
    val_ ~loc "to_rank" [%type: [%t variant_type] -> int]
  ;;

  let v_to_name_fun ~variant_type loc _ =
    val_ ~loc "to_name" [%type: [%t variant_type] -> string]
  ;;

  let variant ~variant_type ~ty_name ~params loc variants =
    let tester_type = [%type: [%t variant_type] -> bool] in
    let helpers, variant_defs =
      List.unzip
        (List.map variants ~f:(fun v ->
           let module V = Variant_constructor in
           let univars = params @ v.V.vars in
           let return_ty = V.return_ty v ~variant_type in
           let constructor_type = V.to_fun_type v ~rhs:return_ty in
           let getter_type_opt = V.to_getter_type v ~lhs:return_ty in
           let name = variant_name_to_string v.V.name in
           ( ( val_ ~loc ~univars name constructor_type
             , val_ ~loc ~univars ("is_" ^ name) tester_type
             , Option.map getter_type_opt ~f:(val_ ~loc ~univars:params (name ^ "_val"))
             )
           , val_ ~loc ~univars name [%type: [%t constructor_type] Variantslib.Variant.t]
           )))
    in
    let constructors, testers, getters = List.unzip3 helpers in
    constructors
    @ testers
    @ List.filter_opt getters
    @ [ psig_module
          ~loc
          (module_declaration
             ~loc
             ~name:(Located.mk ~loc (Some (variants_module ty_name)))
             ~type_:
               (pmty_signature
                  ~loc
                  (variant_defs
                   @ List.filter_opt
                       [ Some (v_fold_fun ~variant_type ~params loc variants)
                       ; Some (v_iter_fun ~variant_type ~params loc variants)
                       ; v_map_fun_opt ~variant_type ~params loc variants
                       ; v_make_matcher_fun_opt ~variant_type ~params loc variants
                       ; Some (v_to_rank_fun ~variant_type ~univars:params loc variants)
                       ; Some (v_to_name_fun ~variant_type ~univars:params loc variants)
                       ; Some (v_descriptions ~variant_type ~univars:params loc variants)
                       ])))
      ]
  ;;

  let variants_of_td td =
    let ty_name = td.ptype_name.txt in
    let loc = td.ptype_loc in
    let params = name_params td.ptype_params in
    let params =
      List.map params ~f:(fun (name, jkind) ->
        Ppxlib_jane.Ast_builder.Default.Latest.ptyp_var ~loc name.txt jkind)
    in
    let variant_type = ptyp_constr ~loc (Located.map lident td.ptype_name) params in
    variant ~variant_type ~ty_name ~params loc (Inspect.type_decl td)
  ;;

  let generate ~loc ~path:_ (rec_flag, tds) =
    (match rec_flag with
     | Nonrecursive ->
       Location.raise_errorf
         ~loc
         "nonrec is not compatible with the `ppx_variants_conv' preprocessor"
     | _ -> ());
    match tds with
    | [ td ] -> variants_of_td td
    | _ -> Location.raise_errorf ~loc "ppx_variants_conv: not supported"
  ;;
end

module Gen_str = struct
  let locally_abstract_match loc cases ~td =
    let params = name_params td.ptype_params in
    let variant_ty =
      ptyp_constr
        ~loc
        (Located.map lident td.ptype_name)
        (List.map params ~f:(fun (name, _) ->
           ptyp_constr ~loc { loc; txt = Longident.Lident name.txt } []))
    in
    let match_expr = pexp_match ~loc [%expr t] cases in
    let fun_expr = [%expr fun (t : [%t variant_ty]) -> [%e match_expr]] in
    List.fold_right params ~init:fun_expr ~f:(fun (name, jkind) acc ->
      Ppxlib_jane.Ast_builder.Default.pexp_newtype ~loc name jkind acc)
  ;;

  let helpers_and_variants loc ~td variants =
    let multiple_cases = List.length variants > 1 in
    let module V = Variant_constructor in
    let helpers, variants =
      List.mapi variants ~f:(fun rank v ->
        let uncapitalized = variant_name_to_string v.V.name in
        let constructor =
          let constructed_value =
            match v.V.kind with
            | `Normal _ ->
              let arg =
                pexp_tuple_opt ~loc (List.map (V.args v) ~f:(fun (_, v) -> evar ~loc v))
              in
              pexp_construct ~loc (Located.lident ~loc v.V.name) arg
            | `Polymorphic _ ->
              let arg =
                pexp_tuple_opt ~loc (List.map (V.args v) ~f:(fun (_, v) -> evar ~loc v))
              in
              pexp_variant ~loc v.V.name arg
            | `Normal_inline_record (fields, _) ->
              let arg =
                pexp_record
                  ~loc
                  (List.map2_exn fields (V.args v) ~f:(fun f (_, name) ->
                     Located.lident ~loc f.pld_name.txt, evar ~loc name))
                  None
              in
              pexp_construct ~loc (Located.lident ~loc v.V.name) (Some arg)
          in
          pstr_value
            ~loc
            Nonrecursive
            [ value_binding
                ~loc
                ~pat:(pvar ~loc uncapitalized)
                ~expr:
                  (List.fold_right
                     (V.args v)
                     ~init:constructed_value
                     ~f:(fun (label, v) e -> pexp_fun ~loc label None (pvar ~loc v) e))
            ]
        in
        let variant =
          [%stri
            let [%p pvar ~loc uncapitalized] =
              { Variantslib.Variant.name = [%e estring ~loc v.V.name]
              ; rank = [%e eint ~loc rank]
              ; constructor = [%e evar ~loc uncapitalized]
              }
            ;;]
        in
        let match_fun ~name ~true_case ~false_expr =
          let cases =
            if multiple_cases
            then [ true_case; case ~guard:None ~lhs:[%pat? _] ~rhs:false_expr ]
            else [ true_case ]
          in
          let body =
            if Variant_constructor.is_gadt v
            then locally_abstract_match loc cases ~td
            else pexp_function ~loc cases
          in
          [%stri let [%p pvar ~loc name] = [%e body] [@@warning "-4"]]
        in
        let tester =
          let name = "is_" ^ uncapitalized in
          let true_case =
            case
              ~guard:None
              ~lhs:(Variant_constructor.pattern_without_binding v)
              ~rhs:[%expr true]
          in
          match_fun ~name ~true_case ~false_expr:[%expr false]
        in
        let getter_opt =
          if not (Variant_constructor.has_getter v)
          then None
          else (
            let name = uncapitalized ^ "_val" in
            let pat, expr = Variant_constructor.to_getter_case v in
            let true_case =
              case ~guard:None ~lhs:pat ~rhs:[%expr Stdlib.Option.Some [%e expr]]
            in
            Some (match_fun ~name ~true_case ~false_expr:[%expr Stdlib.Option.None]))
        in
        (constructor, tester, getter_opt), variant)
      |> List.unzip
    in
    let constructors, testers, getters = List.unzip3 helpers in
    constructors, testers, List.filter_opt getters, variants
  ;;

  let label_arg ?label loc name =
    let l =
      match label with
      | None -> name
      | Some n -> n
    in
    Asttypes.Labelled l, pvar ~loc name
  ;;

  let label_arg_fun loc name = label_arg ~label:name loc (name ^ "_fun__")

  let v_fold_fun loc variants =
    let module V = Variant_constructor in
    let variant_fold acc_expr variant =
      let variant_name = variant_name_to_string variant.V.name in
      [%expr
        [%e evar ~loc (variant_name ^ "_fun__")] [%e acc_expr] [%e evar ~loc variant_name]]
    in
    let body = List.fold_left variants ~init:[%expr init__] ~f:variant_fold in
    let patterns =
      List.map variants ~f:(fun variant ->
        label_arg_fun loc (variant_name_to_string variant.V.name))
    in
    let init = label_arg ~label:"init" loc "init__" in
    let lambda = Create.lambda loc (init :: patterns) body in
    [%stri let fold = [%e lambda]]
  ;;

  let v_descriptions loc variants =
    let module V = Variant_constructor in
    let f v =
      [%expr [%e estring ~loc v.V.name], [%e eint ~loc (List.length (V.args v))]]
    in
    let variant_names = List.map ~f variants in
    [%stri let descriptions = [%e elist ~loc variant_names]]
  ;;

  let v_map_fun_opt loc variants =
    let module V = Variant_constructor in
    (* We are currently not generating getter for GADTs which mean we can't
       generate [map] *)
    if List.exists variants ~f:V.is_gadt
    then None
    else (
      let variant_match_case variant =
        let pattern =
          match variant.V.kind with
          | `Polymorphic _ ->
            let arg =
              ppat_tuple_opt
                ~loc
                (List.map (V.args variant) ~f:(fun (_, v) -> pvar ~loc v))
            in
            ppat_variant ~loc variant.V.name arg
          | `Normal _ ->
            let arg =
              ppat_tuple_opt
                ~loc
                (List.map (V.args variant) ~f:(fun (_, v) -> pvar ~loc v))
            in
            ppat_construct ~loc (Located.lident ~loc variant.V.name) arg
          | `Normal_inline_record (fields, _) ->
            let arg =
              ppat_record
                ~loc
                (List.map2_exn fields (V.args variant) ~f:(fun f (_, v) ->
                   Located.lident ~loc f.pld_name.txt, pvar ~loc v))
                Closed
            in
            ppat_construct ~loc (Located.lident ~loc variant.V.name) (Some arg)
        in
        let uncapitalized = variant_name_to_string variant.V.name in
        let value =
          List.fold_left
            (V.args variant)
            ~init:
              (eapply
                 ~loc
                 (evar ~loc (uncapitalized ^ "_fun__"))
                 [ evar ~loc uncapitalized ])
            ~f:(fun acc_expr (label, var) ->
              pexp_apply ~loc acc_expr [ label, evar ~loc var ])
        in
        case ~guard:None ~lhs:pattern ~rhs:value
      in
      let body = pexp_match ~loc [%expr t__] (List.map variants ~f:variant_match_case) in
      let patterns =
        List.map variants ~f:(fun variant ->
          label_arg_fun loc (variant_name_to_string variant.V.name))
      in
      let lambda = Create.lambda loc ((Nolabel, [%pat? t__]) :: patterns) body in
      Some [%stri let map = [%e lambda]])
  ;;

  let v_iter_fun loc variants =
    let module V = Variant_constructor in
    let names = List.map variants ~f:(fun v -> variant_name_to_string v.V.name) in
    let variant_iter variant =
      let variant_name = variant_name_to_string variant.V.name in
      [%expr
        ([%e evar ~loc (variant_name ^ "_fun__")] [%e evar ~loc variant_name] : unit)]
    in
    let body = esequence ~loc (List.map variants ~f:variant_iter) in
    let patterns = List.map names ~f:(label_arg_fun loc) in
    let lambda = Create.lambda loc patterns body in
    [%stri let iter = [%e lambda]]
  ;;

  let v_make_matcher_fun_opt loc variants =
    let module V = Variant_constructor in
    (* We are currently not generating getter for GADTs which mean we can't
       generate [map] and consequently can't generate a matcher either *)
    if List.exists variants ~f:V.is_gadt
    then None
    else (
      let result =
        let map =
          List.fold_left variants ~init:[%expr map] ~f:(fun acc variant ->
            let variant_name = variant_name_to_string variant.V.name in
            pexp_apply
              ~loc
              acc
              [ ( Labelled variant_name
                , match V.args variant with
                  | [] -> [%expr fun _ -> [%e evar ~loc (variant_name ^ "_gen__")] ()]
                  | _ :: _ -> [%expr fun _ -> [%e evar ~loc (variant_name ^ "_gen__")]] )
              ])
        in
        [%expr [%e map], compile_acc__]
      in
      let body =
        List.fold_right variants ~init:result ~f:(fun variant acc ->
          let variant_name = variant_name_to_string variant.V.name in
          pexp_let
            ~loc
            Nonrecursive
            [ value_binding
                ~loc
                ~pat:
                  (ppat_tuple
                     ~loc
                     [ [%pat? [%p pvar ~loc (variant_name ^ "_gen__")]]
                     ; [%pat? compile_acc__]
                     ])
                ~expr:
                  [%expr
                    [%e evar ~loc (variant_name ^ "_fun__")]
                      [%e evar ~loc variant_name]
                      compile_acc__]
            ]
            acc)
      in
      let patterns =
        List.map variants ~f:(fun v ->
          label_arg_fun loc (variant_name_to_string v.V.name))
      in
      let lambda =
        Create.lambda loc (patterns @ [ Nolabel, [%pat? compile_acc__] ]) body
      in
      Some [%stri let make_matcher = [%e lambda]])
  ;;

  let case_analysis_ignoring_values variants ~f =
    let pattern_and_rhs =
      List.mapi variants ~f:(fun rank v ->
        Variant_constructor.pattern_without_binding v, f ~rank ~name:v.name)
    in
    List.map pattern_and_rhs ~f:(fun (pattern, rhs) -> case ~guard:None ~lhs:pattern ~rhs)
  ;;

  let v_to_rank loc variants ~td =
    let cases =
      case_analysis_ignoring_values variants ~f:(fun ~rank ~name:_ -> eint ~loc rank)
    in
    let body =
      if List.exists variants ~f:Variant_constructor.is_gadt
      then locally_abstract_match loc cases ~td
      else pexp_function ~loc cases
    in
    [%stri let to_rank = [%e body]]
  ;;

  let v_to_name loc variants ~td =
    let cases =
      case_analysis_ignoring_values variants ~f:(fun ~rank:_ ~name -> estring ~loc name)
    in
    let body =
      if List.exists variants ~f:Variant_constructor.is_gadt
      then locally_abstract_match loc cases ~td
      else pexp_function ~loc cases
    in
    [%stri let to_name = [%e body]]
  ;;

  let variant ~variant_name ~td loc ty =
    let constructors, testers, getters, variants = helpers_and_variants loc ~td ty in
    constructors
    @ testers
    @ getters
    @ [ pstr_module
          ~loc
          (module_binding
             ~loc
             ~name:(Located.mk ~loc (Some (variants_module variant_name)))
             ~expr:
               (pmod_structure
                  ~loc
                  (variants
                   @ List.filter_opt
                       [ Some (v_fold_fun loc ty)
                       ; Some (v_iter_fun loc ty)
                       ; v_map_fun_opt loc ty
                       ; v_make_matcher_fun_opt loc ty
                       ; Some (v_to_rank loc ty ~td)
                       ; Some (v_to_name loc ty ~td)
                       ; Some (v_descriptions loc ty)
                       ])))
      ]
  ;;

  let variants_of_td td =
    let variant_name = td.ptype_name.txt in
    let loc = td.ptype_loc in
    variant ~variant_name ~td loc (Inspect.type_decl td)
  ;;

  let generate ~loc ~path:_ (rec_flag, tds) =
    (match rec_flag with
     | Nonrecursive ->
       Location.raise_errorf
         ~loc
         "nonrec is not compatible with the `ppx_variants_conv' preprocessor"
     | _ -> ());
    match tds with
    | [ td ] -> variants_of_td td
    | _ -> Location.raise_errorf ~loc "ppx_variants_conv: not supported"
  ;;
end

let variants =
  Deriving.add
    "variants"
    ~str_type_decl:(Deriving.Generator.make_noarg Gen_str.generate)
    ~sig_type_decl:(Deriving.Generator.make_noarg Gen_sig.generate)
;;
