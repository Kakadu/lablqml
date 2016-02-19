open Sexplib.Conv
open Core_kernel.Std

type cpptype = { t_name:string; t_is_const:bool; t_indirections:int; t_is_ref:bool; t_params: cpptype list }
and func_arg = { arg_type:cpptype; arg_name:string option; arg_default: string option }
and meth = {
  m_res:cpptype;
  m_name:string;
  m_args:func_arg list;
  m_declared: string;
  m_out_name:string;
  m_access:[`Public | `Protected| `Private];
  m_modif :[`Static | `Abstract | `Const | `Virtual | `Explicit | `Inline ] list
}
[@@deriving sexp]

let void_type = {t_name="void"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
let qmodelindex_type =
  {t_name="QModelIndex"; t_is_const=false; t_indirections=0; t_is_ref=true; t_params=[] }
let int_type      = {t_name="int";      t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
let bool_type     = {t_name="bool";     t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
let qvariant_type = {t_name="QVariant"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
let qpoint_type   = {t_name="QPoint";   t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
let string_type   = {t_name="QString";  t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
let bytearray_type = {string_type with t_name="QByteArray"}

let remove_defaults {m_res;m_name; m_args; m_declared; m_out_name; m_access; m_modif } =
  let m_args = List.map m_args ~f:(fun x -> {x with arg_default=None} ) in
  {m_res;m_name; m_declared; m_args; m_out_name; m_access; m_modif }

let unreference = function
  | {t_name=t_name; t_indirections=t_indirections; t_is_const=t_is_const; t_params=t_params; _} ->
    let t_is_ref = false in
    { t_name; t_indirections; t_is_const; t_is_ref; t_params }

let is_void_type t = (t.t_name = "void") && (t.t_indirections=0)
let is_bool_type t = (t.t_name = "bool") && (t.t_indirections=0)

let meth_of_constr ~classname m_args =
  let m_declared = classname and m_name=classname and m_out_name=classname
  and m_res={ t_name=classname; t_indirections=1; t_is_ref=false; t_params=[]; t_is_const=false } in
  { m_declared; m_name; m_args; m_res; m_out_name; m_access=`Public; m_modif=[] }

let rec string_of_type t =
  let b = Buffer.create 10 in
  if t.t_is_const then Buffer.add_string b "const ";
  Buffer.add_string b t.t_name;
  let () =
    match t.t_params with
      | [] -> ()
      | [x] ->
          Buffer.add_char b '<';
          Buffer.add_string b (string_of_type x);
          Buffer.add_char b '>'
      | x::xs ->
          Buffer.add_char b '<';
          Buffer.add_string b (string_of_type x);
          List.iter xs ~f:(fun x -> Buffer.add_char b ','; Buffer.add_string b (string_of_type x) );
          Buffer.add_char b '>';
  in
  Buffer.add_string b (String.make t.t_indirections '*');
  if t.t_is_ref then Buffer.add_string b " &";
  Buffer.contents b
