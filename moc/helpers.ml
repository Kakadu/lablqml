open Core
module String = Core_string
module S = Std_internal

module List = struct 
  include Core_list
  let iter2i l1 l2 ~f =
    let i = ref 0 in
    Core_list.iter2_exn l1 l2 ~f: (fun x y -> f !i x y; incr i)
end

let classname = "UserSlots"
let ocaml_classname = 
  let s = String.copy classname in
  s.[0] <- Core_char.lowercase s.[0];
  s

let to_cpp_type = function
  | "float" -> "double"
  | "unit"  -> "void"
  | "int"
  | "bool" as t -> t
  | s when s.[0] = 'q' -> let s = String.copy s in s.[0]<-'Q' ; s ^ "*"
  | _ -> assert false

let is_qt_classname s = (s.[0]='q') || (s.[0]='Q')

let to_qt_classname s = 
  if is_qt_classname s then (
    let s' = String.copy s in
    s'.[0] <- 'Q';
    s'
  ) else 
    Std_internal.failwithf "Bad arguments of to_qt_classname" ()

let name_for_slot (name,lst) = 
  let f = String.map ~f:(function '*' -> '_' | s -> s) in
  String.concat ~sep:"_" (ocaml_classname::name::(List.map lst ~f))


let to_cpp_type s = match s with
  | "int"  -> "int"
  | "bool" -> "bool"
  | "unit" -> "void"
  | "string"
  | "qstring" 
  | "qString" -> "QString"
  | s when is_qt_classname s -> to_qt_classname s ^ "*"
  | _ -> assert false

