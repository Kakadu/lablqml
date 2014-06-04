open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

let getenv s = try Sys.getenv s with Not_found -> ""

exception Error of Location.t
exception Error2 of string * Location.t

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error loc ->
      Some (error ~loc " QWE [%getenv] accepts a string, e.g. [%getenv \"USER\"]")
    | Error2 (msg,loc) -> Some (error ~loc msg)
    | _ -> None)

let (_: Ast_mapper.mapper) = default_mapper

let wrap_class_type_decl mapper (ct: Parsetree.class_type Parsetree.class_infos) =
  print_endline "wrap_class_type_decl on class type markend with `qtclass`";
  let clas_sig = match ct.pci_expr.pcty_desc with
  | Pcty_signature s -> s
  | Pcty_constr _    -> raise (Error2 ("???", ct.pci_loc))
  | Pcty_extension _ -> raise (Error2 ("???", ct.pci_loc))
  | Pcty_arrow _     -> raise (Error2 ("???", ct.pci_loc))
  in
  let fields: class_type_field list = clas_sig.pcsig_fields in
  let wrap_meth ((name,_,_,typ) as m) = [Pctf_method m] in
  let wrap_prop ((name,_,_,typ) as v) = [Pctf_val v] in
  let wrap_field (f_desc: class_type_field) : class_type_field list =
    let ans = match f_desc.pctf_desc with
      | Pctf_val v    -> wrap_prop v
      | Pctf_method m -> wrap_meth m
      (* values become properties and methods become Q_INVOKABLE methods. Other values not changed *)
      | Pctf_inherit _ -> []
      | Pctf_constraint _ -> []
      | Pctf_attribute _ -> []
      | Pctf_extension _ -> []
    in
    let ans' = List.map (fun ans -> { f_desc with pctf_desc=ans }) ans in
    ans'
  in
  let new_fields = List.map wrap_field fields |> List.concat in
  let new_desc = Pcty_signature {clas_sig with pcsig_fields = new_fields} in
  let new_expr = {ct.pci_expr with pcty_desc = new_desc } in
  let ans = {ct with pci_expr=new_expr} in
  ans
(*
  default_mapper.class_type_declaration mapper ct
 *)
let getenv_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  {{ default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
          (* Should have name "getenv". *)
          Pexp_extension ({ txt = "getenv"; loc }, pstr) } ->
        begin match pstr with
        | (* Should have a single structure item, which is evaluation of a constant string. *)
          PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc =
                               Pexp_constant (Const_string (sym, None))}, _)}] ->
          (* Replace with a constant string with the value from the environment. *)
          Exp.constant ~loc (Const_string (getenv sym, None))
        | _ -> raise (Error loc)
        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  } with class_type_declaration = fun mapper ct ->
      print_endline "qtclass  is found";
      match ct.pci_attributes with
      | [({txt="qtclass"; loc},_)] -> wrap_class_type_decl mapper ct
      | _ -> default_mapper.class_type_declaration mapper ct
(*
      match match ct.pci_expr.pcty_desc with
  | Pcty_signature _ -> print_endline "sig"
  | Pcty_constr _ -> print_endline "constr"
  | Pcty_extension _ -> print_endline "ext"
  | Pcty_arrow _ -> print_endline "arrow"
  in
      wrap_class_type_decl mapper ct
(*
      match ct with
      | [
(*
      | {pcty_desc=Pcty_extension({txt="qtclass"; loc},_); pcty_attributes; _} ->
         print_endline "qtclass  is found";
         default_mapper.class_type mapper ct
 *)
      | _ -> print_endline "DEFAULT MAPPER";
             default_mapper.class_type_declaration mapper ct
 *)
 *)
  }


let () = run_main getenv_mapper
