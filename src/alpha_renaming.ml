open Base
open Ppxlib

type t = string Map.M(String).t

let empty = Map.empty (module String)

let var t var =
  match Map.find t var with
  | None ->
    let fresh = Ppxlib.gen_symbol ~prefix:var () in
    Map.set t ~key:var ~data:fresh, fresh
  | Some var -> t, var
;;

let any t =
  let fresh = Ppxlib.gen_symbol ~prefix:"unnamed" () in
  Map.set t ~key:fresh ~data:fresh, fresh
;;

let fold_map =
  object
    inherit [t] Ast_traverse.fold_map as super

    method! core_type ctype t =
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ctype.ptyp_desc with
      | Ptyp_var (name, jkind) ->
        let t, name = var t name in
        ( { ctype with
            ptyp_desc =
              Ppxlib_jane.Shim.Core_type_desc.to_parsetree (Ptyp_var (name, jkind))
          }
        , t )
      | Ptyp_any jkind ->
        let t, name = any t in
        ( { ctype with
            ptyp_desc =
              Ppxlib_jane.Shim.Core_type_desc.to_parsetree (Ptyp_var (name, jkind))
          }
        , t )
      | _ -> super#core_type ctype t

    method! constructor_declaration cd t =
      let pcd_vars =
        Ppxlib_jane.Shim.Constructor_declaration.extract_vars_with_jkind_annotations cd
      in
      let t, pcd_vars =
        List.fold_map pcd_vars ~init:t ~f:(fun t ({ txt; loc }, jkind) ->
          let t, txt = var t txt in
          t, ({ txt; loc }, jkind))
      in
      let cd =
        Ppxlib_jane.Shim.Constructor_declaration.create
          ~name:cd.pcd_name
          ~vars:pcd_vars
          ~args:cd.pcd_args
          ~res:cd.pcd_res
          ~loc:cd.pcd_loc
      in
      super#constructor_declaration cd t
  end
;;

let range t = Map.data t
