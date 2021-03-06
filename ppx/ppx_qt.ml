open Base
open Printf
open PpxQtCfg
open Gencpp
open Ppxlib
open Ppxlib.Ast_builder.Default
open TypeRepr

let make_coretyp ~loc txt = Ast_helper.Typ.constr ~loc { txt; loc } []

let cppobj_coretyp loc = make_coretyp ~loc (Lident "cppobj")

let unit_coretyp loc = make_coretyp ~loc (Lident "unit")

let int_coretyp loc = make_coretyp ~loc (Lident "int")

let string_coretyp loc = make_coretyp ~loc (Lident "string")

let make_store_func ~loc ~classname : structure_item =
  let pval_prim = [ sprintf "caml_store_value_in_%s" classname ] in
  let pval_type =
    [%type:
      t Lablqml.cppobj ->
      [%t ptyp_object ~loc [] Open] ->
      [%t ptyp_constr ~loc { txt = Lident "unit"; loc } []]]
  in
  pstr_primitive ~loc
  @@ value_description ~loc ~name:(Located.mk ~loc "store") ~type_:pval_type
       ~prim:pval_prim

let make_stub_general ~loc ~types ~name ~stub_name =
  let rec helper = function
    | [] -> assert false
    | [ t ] -> t
    | txt :: xs -> [%type: [%t txt] -> [%t helper xs]]
  in
  let type_ = helper types in
  pstr_primitive ~loc
  @@ value_description ~loc ~name:(Located.mk ~loc name) ~type_
       ~prim:[ stub_name ]

let make_creator ~loc ~classname =
  pstr_primitive ~loc
  @@ value_description ~loc
       ~name:(Located.mk ~loc @@ sprintf "create_%s" classname)
       ~type_:[%type: unit -> 'a]
       ~prim:[ sprintf "caml_create_%s" classname ]

let make_stub_for_signal ~classname ~loc ~typ name : structure_item =
  pstr_primitive ~loc
  @@ value_description ~loc
       ~name:(Located.mk ~loc @@ sprintf "stub_%s" name)
       ~type_:[%type: _ Lablqml.cppobj -> [%t typ] -> unit]
       ~prim:[ sprintf "caml_%s_%s_cppmeth_wrapper" classname name ]

let make_virt_meth ~loc ~name xs =
  let rec helper = function
    | [] -> assert false
    | [ t ] -> ptyp_constr ~loc { txt = TypeRepr.ocaml_ast_of_typ t; loc } []
    | t :: xs ->
        [%type:
          [%t
            ptyp_constr ~loc (Located.mk ~loc @@ TypeRepr.ocaml_ast_of_typ t) []] ->
          [%t helper xs]]
  in
  let typ = helper xs in
  pcf_method ~loc (Located.mk name ~loc, Public, Cfk_virtual typ)

let mkloc x loc = Located.mk ~loc x

let make_initializer ~loc : class_field =
  pcf_initializer ~loc [%expr store cppobj self]

let make_handler_meth ~loc : class_field =
  let e = [%expr cppobj] in
  pcf_method ~loc (Located.mk "handler" ~loc, Public, Cfk_concrete (Fresh, e))

let eval_meth_typ t =
  match TypeRepr.eval_meth_typ_gen t with
  | Result.Ok xs -> List.map ~f:snd xs
  | Error (msg, typ) -> raise @@ ErrorMsg (msg, typ)

let eval_signal_typ t =
  match TypeRepr.eval_meth_typ_gen t with
  | Result.Ok xs -> xs
  | Error (msg, typ) -> raise @@ ErrorMsg (msg, typ)

let check_meth_typ ~loc _xs =
  let _ = loc in
  (* TODO: some checks like unit type should be at the end of list *)
  (* TODO: check that modelindexes are not used without QAbstractItemModel base *)
  true

let wrap_meth ~classname (* ?(options = []) *)
    (({ txt = methname; loc }, _, kind) as m) =
  match kind with
  | Cfk_concrete _ ->
      raise @@ ErrorMsg ("Qt methods should be marked as virtual", loc)
  | Cfk_virtual typ ->
      let meth_typ = eval_meth_typ typ in
      if not (check_meth_typ ~loc meth_typ) then
        raise @@ ErrorMsg (sprintf "Method '%s' has wrong type" methname, loc);
      let () =
        if PpxQtCfg.config.gencpp then
          (* let options =
               if Options.is_itemmodel options then [ OItemModel ] else []
             in *)
          Gencpp.gen_meth (*~options*) ~classname ~methname
            (meth_typ :> Arg.non_cppobj Arg.t list)
      in
      [ pcf_method ~loc m ]

(*
(* in 4.03 definition have changed from string to Ast_types.arg_label *)
let oldify_arg_label = function
  | Nolabel -> ""
  | Labelled s -> s
  | Optional s -> s
;; *)

module OfClass = struct
  let run ~attributes loc (ci : class_declaration) =
    (* print_endline "wrap_class_type_decl on class type markend with `qtclass`"; *)
    let classname = ci.pci_name.txt in
    let options =
      List.concat
        [
          (if has_attr "itemmodel" attributes then [ OItemModel ] else []);
          (if has_attr "instantiable" attributes then [ OInstantiable ] else []);
        ]
    in
    if PpxQtCfg.config.gencpp then Gencpp.open_files ~options ~classname;
    let clas_sig =
      match ci.pci_expr.pcl_desc with
      | Pcl_structure s -> s
      | _ ->
          raise
          @@ ErrorMsg
               ("Qt class signature should be structure of class", ci.pci_loc)
    in
    let fields : class_field list = clas_sig.pcstr_fields in
    let heading = ref [ [%stri type t]; make_store_func ~classname ~loc ] in
    let wrap_signal ~options ~classname
        (({ txt = signalname; loc }, _, kind) as _m) =
      let _ = options in
      match kind with
      | Cfk_concrete _ ->
          raise
          @@ ErrorMsg ("We can generate prop methods for virtuals only", loc)
      | Cfk_virtual core_typ ->
          (* stub which will be called by OCaml meth*)
          let external_stub =
            let open Ast_builder.Default in
            pstr_primitive ~loc
            @@ value_description ~loc
                 ~name:(Located.mk ~loc @@ sprintf "stub_%s" signalname)
                 ~type_:[%type: [%e t Lablqml.cppobj loc] -> [%e core_typ]]
                 ~prim:
                   [ sprintf "caml_%s_%s_emitter_wrapper" classname signalname ]
          in
          Gencpp.ref_append ~set:heading external_stub;
          (* C++ stub *)
          let types = eval_signal_typ core_typ in
          let args, res = List.(drop_last_exn types, last_exn types) in
          if Stdlib.(snd res <> TypeRepr.Arg.Unit) then
            raise @@ ErrorMsg ("Result type for signal should be unit", loc);
          assert (Stdlib.(fst res = Nolabel));
          (* last argument always will be without a label, isn't it? *)
          if List.exists ~f:(fun (label, _) -> Stdlib.( = ) label Nolabel) args
          then raise @@ ErrorMsg ("All arguments should have a label", loc);
          if config.gencpp then
            Gencpp.gen_signal ~classname ~signalname
            @@ List.map ~f:(fun (l, x) -> (l, (x :> Arg.non_cppobj Arg.t))) args;
          (* OCaml meth *)
          let e =
            pexp_poly ~loc
              (pexp_apply ~loc
                 (pexp_ident ~loc
                    (Located.mk ~loc (Lident ("stub_" ^ signalname))))
                 [ (Nolabel, [%expr self#handler]) ])
              None
          in
          [
            pcf_method ~loc
              ( Located.mk ~loc ("emit_" ^ signalname),
                Public,
                Cfk_concrete (Fresh, e) );
          ]
    in
    let wrap_prop ~classname (loc, flag, kind) =
      let propname = loc.txt in
      let loc = loc.loc in
      match kind with
      | Cfk_concrete _ ->
          raise
          @@ ErrorMsg ("We can generate prop methods for virtuals only", loc)
      | Cfk_virtual core_typ -> (
          match type_suits_prop core_typ with
          | Ok typ ->
              if config.gencpp then Gencpp.gen_prop ~classname ~propname typ;
              let signal_name = Names.signal_of_prop propname in
              ref_append ~set:heading
                (make_stub_for_signal ~classname ~loc ~typ:core_typ signal_name);
              let e =
                pexp_poly ~loc
                  (pexp_apply ~loc
                     (pexp_ident ~loc
                        (Located.mk ~loc @@ lident ("stub_" ^ signal_name)))
                     [ (Nolabel, [%expr self#handler]) ])
                  None
              in
              [
                pcf_method ~loc
                  ( Located.mk ~loc ("emit_" ^ signal_name),
                    Public,
                    Cfk_concrete (Fresh, e) );
                pcf_method ~loc
                  ( Located.mk ~loc (Gencpp.Names.getter_of_prop propname),
                    flag,
                    Cfk_virtual
                      Ast_helper.Typ.(arrow Nolabel (unit_coretyp loc) core_typ)
                  );
              ]
          | Error msg ->
              raise
              @@ ErrorMsg
                   (sprintf "Can't wrap property '%s': %s" propname msg, loc))
    in
    let wrap_field (f_desc : class_field) : class_field list =
      match f_desc.pcf_desc with
      | Pcf_method m when has_attr "qtmeth" f_desc.pcf_attributes ->
          wrap_meth ~classname m
      | Pcf_method m when has_attr "qtsignal" f_desc.pcf_attributes ->
          wrap_signal ~options ~classname m
      | Pcf_method m when has_attr "qtprop" f_desc.pcf_attributes ->
          wrap_prop ~classname m
      | _ -> []
    in
    let ocaml_typ_of_typ cppobj_param =
      let open TypeRepr.Arg in
      let rec helper = function
        | Cppobj ->
            ptyp_constr ~loc
              (Located.mk ~loc @@ Ldot (Lident "Lablqml", "cppobj"))
              [ cppobj_param ]
        | QVariant -> [%type: QVariant.t]
        | QModelIndex -> [%type: QModelIndex.t]
        | Bool -> [%type: bool]
        | Unit -> [%type: unit]
        | QByteArray | QString -> [%type: string]
        | Int -> [%type: int]
        | QList x ->
            ptyp_constr ~loc (Located.mk ~loc @@ Lident "list") [ helper x ]
      in

      helper
    in
    let itemmodel_meths =
      if has_attr "itemmodel" attributes then (
        let f (methname, meth_typ, minfo) =
          (* printf "Generating itemmodel-specific meth: '%s'\n" methname; *)
          if config.gencpp then
            Gencpp.gen_meth ~classname ~methname ~minfo meth_typ
        in
        if config.gencpp then List.iter ~f Gencpp.itemmodel_members;
        (* now add some OCaml code *)
        let f (name, stub_name, xs) =
          let types = List.map xs ~f:(ocaml_typ_of_typ [%type: t]) in
          ref_append ~set:heading
          @@ make_stub_general ~loc ~types ~name:("stub_" ^ name) ~stub_name
        in
        List.iter (Gencpp.itemmodel_externals ~classname) ~f;
        let () = if config.gencpp then Gencpp.gen_itemmodel_stuff ~classname in
        let add_role_stub =
          let name = Located.mk ~loc "add_role" in
          let prim =
            [ sprintf "caml_%s_%s_cppmeth_wrapper" classname "addRole" ]
          in
          let type_ = [%type: 'a -> int -> string -> unit] in
          Ast_builder.Default.pstr_primitive ~loc
          @@ Ast_builder.Default.value_description ~loc ~name ~type_ ~prim
        in
        ref_append add_role_stub ~set:heading;

        let emitters =
          List.map
            [
              "dataChanged";
              "beginInsertRows";
              "endInsertRows";
              "beginRemoveRows";
              "endRemoveRows";
            ] ~f:(fun name ->
              let e =
                pexp_poly ~loc
                  (pexp_apply ~loc
                     (pexp_ident ~loc @@ Located.mk ~loc
                     @@ Lident ("stub_" ^ name))
                     [ (Nolabel, [%expr cppobj]) ])
                  None
              in

              pcf_method ~loc (mkloc name loc, Public, Cfk_concrete (Fresh, e)))
        in
        let virtuals =
          [
            make_virt_meth
              [ Arg.QModelIndex; Arg.QModelIndex ]
              ~loc ~name:"parent";
            make_virt_meth
              [ Arg.Int; Arg.Int; Arg.QModelIndex; Arg.QModelIndex ]
              ~loc ~name:"index";
            make_virt_meth [ Arg.QModelIndex; Arg.Int ] ~loc ~name:"columnCount";
            make_virt_meth [ Arg.QModelIndex; Arg.Int ] ~loc ~name:"rowCount";
            make_virt_meth
              [ Arg.QModelIndex; Arg.Bool ]
              ~loc ~name:"hasChildren";
            make_virt_meth
              [ Arg.QModelIndex; Arg.Int; Arg.QVariant ]
              ~loc ~name:"data";
          ]
        in
        emitters @ virtuals)
      else []
    in
    let new_fields = List.concat_map fields ~f:wrap_field @ itemmodel_meths in
    let new_fields =
      make_initializer ~loc :: make_handler_meth ~loc :: new_fields
    in
    let new_expr =
      let open Ast_builder.Default in
      pcl_fun ~loc Nolabel None [%pat? cppobj]
      @@ pcl_structure ~loc
      @@ class_structure ~self:[%pat? self] ~fields:new_fields
    in
    let ans = pstr_class ~loc [ { ci with pci_expr = new_expr } ] in
    let creator = make_creator ~loc ~classname in
    if config.gencpp then Gencpp.close_files ~options ();
    !heading @ [ ans; creator ]
end

let () =
  Ppxlib.Driver.register_transformation
    ~impl:(fun ss ->
      let m =
        object (self)
          inherit Ast_traverse.map as super

          method! structure ss =
            (* TODO: Maybe we don't need this *)
            List.concat @@ List.map ~f:self#do_structure_item ss

          method do_structure_item si =
            let ans =
              let open Ast_pattern in
              parse
                (alt
                   (pstr_module
                    @@ module_binding ~name:(some __)
                         ~expr:
                           (pmod_constraint
                              (pmod_attributes __ @@ pmod_structure __)
                              (pmty_signature __))
                   |> pack3
                   |> map2 ~f:(fun (name, attrs, stru) sign ->
                          match find_attr ~name:"qml" attrs with
                          | Some (PStr [ { pstr_desc = Pstr_eval (e, _) } ])
                            -> (
                              match Myparser.Testdemo.parse_singleton e with
                              | None ->
                                  raise
                                    (ErrorMsg ("bad attribute", si.pstr_loc))
                              | Some info ->
                                  Generation2.wrap_module_decl ~loc:si.pstr_loc
                                    name stru sign info)
                          | Some _ ->
                              raise
                                (ErrorMsg
                                   ( sprintf "bad attribute %s %d" __FILE__
                                       __LINE__,
                                     si.pstr_loc ))
                          | None -> [ super#structure_item si ]))
                   (pstr_class (__ ^:: nil)
                   |> map1 ~f:(fun cinfo ->
                          if has_attr "qtclass" cinfo.pci_attributes then
                            Ast_helper.with_default_loc si.pstr_loc (fun () ->
                                OfClass.run ~attributes:cinfo.pci_attributes
                                  si.pstr_loc cinfo)
                          else [ super#structure_item si ])))
                si.pstr_loc
                ~on_error:(fun () -> [ super#structure_item si ])
                si Fun.id
            in
            ans
        end
      in
      m#structure ss)
    "ppx_qt"
