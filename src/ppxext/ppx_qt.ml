open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location
open Printf
open Gencpp

type options = { mutable gencpp: bool }
let options  = { gencpp=true }

let () =
  let specs = [ ("-nocpp", Arg.Unit (fun () -> options.gencpp <- false), "Don't generate C++")] in
  Arg.parse specs (fun _ -> ())
            "usage there"


let rec has_attr name: Parsetree.attributes -> bool = function
  | [] -> false
  | ({txt;loc},_) :: _ when txt = name  -> true
  | _ :: xs -> false

let getenv s = try Sys.getenv s with Not_found -> ""

exception Error of Location.t
exception ErrorMsg of string * Location.t

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error loc ->
      Some (error ~loc " QWE [%getenv] accepts a string, e.g. [%getenv \"USER\"]")
    | ErrorMsg (msg,loc) -> Some (error ~loc msg)
    | _ -> None)

let (_: Ast_mapper.mapper) = default_mapper

let type_suits_prop ty = match ty.ptyp_desc with
  | Ptyp_constr ({txt=Lident "int";    _},[]) -> `Ok `int
  | Ptyp_constr ({txt=Lident "bool";   _},[]) -> `Ok `bool
  | Ptyp_constr ({txt=Lident "string"; _},[]) -> `Ok `string
  | Ptyp_constr ({txt=Lident "unit"; _},[]) -> `Error "property can't have type 'unit'"
  | Ptyp_constr ({txt=Lident x; _},[]) -> `Error (sprintf "Don't know what to do with '%s'" x)
  | _ -> `Error "Type is unknown"


let cppobj_coretyp loc =
  Ast_helper.Typ.constr ~loc ({txt=Lident "cppobj"; loc}) []

let unit_coretyp loc =
  Ast_helper.Typ.constr ~loc ({txt=Lident "unit"; loc}) []

let make_store_func ~loc ~classname : structure_item =
  let pval_name = {txt="store"; loc} in
  let pval_prim = [sprintf "caml_store_value_in_%s" classname] in
  let pval_type = Ast_helper.Typ.(arrow ""
                                        (cppobj_coretyp loc)
                                        (arrow ""
                                               (object_ ~loc [] Open)
                                               (constr ~loc {txt=Lident "unit"; loc} [] ) ) )
  in
  let pstr_desc = Pstr_primitive {pval_name; pval_type; pval_prim; pval_attributes=[];
                                  pval_loc=loc } in
  { pstr_desc; pstr_loc=loc }


let make_stub_for_signal ~classname ~loc ~typ name : structure_item =
(*  let open Ast_helper in*)
  let pval_name = {txt="stub_"^name; loc} in
  let pval_prim = [sprintf "caml_%s_%s_cppmeth_wrapper" classname name] in
  let pval_type = Ast_helper.Typ.(arrow ""
                                        (cppobj_coretyp loc)
                                        (arrow "" typ
                                               (constr ~loc {txt=Lident "unit"; loc} [] ) ) )
  in
  let pstr_desc = Pstr_primitive {pval_name; pval_type; pval_prim; pval_attributes=[];
                                  pval_loc=loc } in
  { pstr_desc; pstr_loc=loc }


let make_initializer ~loc : class_field =
  let pexp_desc = Ast_helper.Exp.(
    apply (ident (mkloc (Lident "store") loc))
          [ ("",ident (mkloc (Lident "cppobj") loc))
          ; ("",ident (mkloc (Lident "self") loc))
          ]
          )
  in (* TODO: *)
  let pcf_desc = Pcf_initializer pexp_desc in
  { pcf_desc; pcf_loc=loc; pcf_attributes=[] }

let make_handler_meth ~loc : class_field =
  let e = Ast_helper.Exp.(poly (ident (mkloc (Lident "cppobj") loc) ) None) in
  Ast_helper.(Cf.method_ (mkloc "handler" loc) Public (Cfk_concrete (Fresh,e)  ) )

let eval_meth_typ t =
  let rec parse_one t = match t.ptyp_desc with
    | Ptyp_constr (({txt=Lident "list"  ;_}),[typarg]) -> `list (parse_one typarg)
    | Ptyp_constr (({txt=Lident "string";_}),_) -> `string
    | Ptyp_constr (({txt=Lident "unit"  ;_}),_) -> `unit
    | Ptyp_constr (({txt=Lident "int"   ;_}),_) -> `int
    | _ -> raise @@ ErrorMsg ("can't eval type", t.ptyp_loc)
  in
  let rec helper t =
    match t.ptyp_desc with
    | Ptyp_arrow (_,l,r) -> [parse_one l] @ (helper r)
    | x -> [parse_one t] (*raise @@ ErrorMsg ("don't know what to do with this type", t.ptyp_loc)*)
  in
  helper t

let check_meth_typ ~loc _xs =
  (* TODO: some checks like unit type should be at the end of list *)
  true

let wrap_meth ~classname (({txt=methname; loc},_,kind) as m) =
  match kind with
  | Cfk_concrete _ -> raise @@ ErrorMsg ("Qt methods should be marked as virtual", loc)
  | Cfk_virtual typ -> begin
     let meth_typ = eval_meth_typ typ in
     if not (check_meth_typ ~loc meth_typ)
     then raise @@ ErrorMsg (sprintf "Method '%s' has wrong type" methname, loc);
     if options.gencpp then Gencpp.gen_meth ~classname ~methname meth_typ; (* TODO: Warning? *)
     [Pcf_method m]
  end

let wrap_class_decl ?(destdir=".") ~attributes mapper loc (ci: class_declaration) =
  (*print_endline "wrap_class_type_decl on class type markend with `qtclass`";*)
  let classname = ci.pci_name.txt in
  if options.gencpp then Gencpp.open_files ~destdir ~classname;
  let clas_sig = match ci.pci_expr.pcl_desc with
    | Pcl_structure s -> s
    |  _    -> raise @@ ErrorMsg ("Qt class signature should be structure of class", ci.pci_loc)
  in
  let fields: class_field list = clas_sig.pcstr_fields in

  let heading = ref [] in
  Ref.append ~set:heading (make_store_func ~classname ~loc);

  let wrap_prop ~classname (loc,flag,kind) =
    let propname = loc.txt in
    let l = loc.loc in
    match kind with
    | Cfk_concrete _ ->
       raise @@ ErrorMsg ("We can generate prop methods for virtuals only", !default_loc)
    | Cfk_virtual core_typ -> begin
       match type_suits_prop core_typ with
       | `Ok typ ->
          if options.gencpp then Gencpp.gen_prop ~classname ~propname typ;
          let signal_name = Names.signal_of_prop propname in
          Ref.append ~set:heading (make_stub_for_signal ~classname ~loc:l ~typ:core_typ signal_name);
          let open Ast_helper in
          let e = Exp.(poly (apply
                               (ident (mkloc (Lident ("stub_"^signal_name)) l) )
                               ["",send (ident (mkloc (Lident "self") l)) "handler"
                               ]
                            )
                            None) in

          [ (Cf.method_ (mkloc ("emit_" ^ signal_name) l)
                      Public (Cfk_concrete (Fresh,e)) ).pcf_desc
          ; Pcf_method ({loc with txt=Names.signal_of_prop propname},
                        flag,
                        Cfk_virtual Ast_helper.Typ.(arrow "" (unit_coretyp l) core_typ) )
          ]
       | `Error msg ->
          raise @@ ErrorMsg (sprintf "Can't wrap property '%s': %s" propname msg, l)
    end
  in
  let wrap_field (f_desc: class_field) : class_field list =
    let ans = match f_desc.pcf_desc with
      | Pcf_method m  when has_attr "qtmeth" f_desc.pcf_attributes ->
         wrap_meth ~classname m
      | Pcf_method m  when has_attr "qtprop" f_desc.pcf_attributes ->
         wrap_prop ~classname m
      | _ -> []
    in
    let ans' = List.map (fun ans -> { f_desc with pcf_desc=ans }) ans in
    ans'
  in
  let new_fields = fields |> List.map ~f:wrap_field  |> List.concat in
  if has_attr "itemmodel" attributes then (

  );
  if options.gencpp then Gencpp.close_files ();

  let new_fields = (make_initializer loc) :: (make_handler_meth ~loc) :: new_fields in
  let new_desc = Pcl_structure { pcstr_fields = new_fields;
                                 pcstr_self = Ast_helper.Pat.var (mkloc "self" loc) } in
  let new_expr = {ci.pci_expr with pcl_desc = new_desc } in
  let new_desc2 = Pcl_fun ("", None, Ast_helper.Pat.var (mkloc "cppobj" loc), new_expr) in
  let new_expr2 = {new_expr with pcl_desc = new_desc2 } in

  let ans = { pstr_desc=Pstr_class [{{ci with pci_expr=new_expr2} with pci_attributes=[]}]
            ; pstr_loc=loc } in
  !heading @ [ans]


let getenv_mapper argv =
  { default_mapper with
    structure = fun mapper items ->
      let rec iter items =
        match items with
        | { pstr_desc=Pstr_class [cinfo]; pstr_loc } as item :: rest when
               has_attr "qtclass" cinfo.pci_attributes ->
           Ast_helper.with_default_loc pstr_loc (fun () ->
             wrap_class_decl ~attributes:cinfo.pci_attributes mapper pstr_loc cinfo @ iter rest )
        | { pstr_loc } as item :: rest ->
          mapper.structure_item mapper item :: iter rest
        | [] -> []
      in
      iter items
  }

let () = run_main getenv_mapper
