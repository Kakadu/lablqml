open Core.Std

module List = struct 
  include List
  let iter2i l1 l2 ~f =
    let i = ref 0 in
    List.iter2_exn l1 l2 ~f: (fun x y -> f !i x y; incr i)
end

let classname = "UserSlots"
let ocaml_classname = 
  let s = String.copy classname in
  s.[0] <- Char.lowercase s.[0];
  s
(*
let to_cpp_type = function
  | "float" -> "double"
  | "unit"  -> "void"
  | "int"
  | "bool" as t -> t
  | s when s.[0] = 'q' -> let s = String.copy s in s.[0]<-'Q' ; s ^ "*"
  | _ -> assert false
      *)
let is_qt_classname s = (s.[0]='q') || (s.[0]='Q')

let to_qt_classname s = 
  if is_qt_classname s then (
    let s' = String.copy s in
    s'.[0] <- 'Q';
    s'
  ) else 
    failwithf "Bad arguments of to_qt_classname" ()


let rec to_cpp_type s = match s with
  | `Simple "int"  -> "int"
  | `Simple "bool" -> "bool"
  | `Simple "unit" -> "void"
  | `Simple "string"
  | `Simple "qstring" 
  | `Simple "qString" -> "QString"
  | `Simple s when is_qt_classname s -> to_qt_classname s ^ "*"
  | `List s ->  sprintf "QList<%s>" (to_cpp_type (`Simple s) )
  | _ -> assert false

let name_for_slot (name,lst,res) =
  let f = String.map ~f:(function '*' -> '_' | s -> s) in
  let conv = function
    | `Simple s -> s
    | `List s -> sprintf "%s_list" s
  in
  let lst = List.map  lst ~f:(fun x -> f (conv x)) in
  String.concat ~sep:"_" (ocaml_classname :: name :: (conv res) :: lst)
