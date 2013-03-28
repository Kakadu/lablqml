type t

module M = Map.Make(String)

let container: t M.t ref = ref M.empty

let add_view name (v: t) =
  container := M.add name v !container
let () =
  Callback.register "register_view" add_view

let get_view_exn ~name = M.find name !container

let get_view ~name =
  try Some (get_view_exn name)
  with Not_found -> None

type cppobj = [ `cppobject ]
external set_context_property: ctx:t -> name:string -> cppobj -> unit
  = "caml_setContextProperty"
external set_caml_object: cppobj -> < .. > -> unit = "caml_set_caml_object"

module QVariant = struct
  type t = [ `empty | `string of string | `qobject of cppobj ]
  let empty = `empty
  let of_string s = `string s
  let of_object o = `qobject o
end

module QModelIndex = struct
  type t = int * int
  let empty = (-1,-1)
  let row = fst
  let column = snd
  let make ~row ~column = (row,column)
  let to_string (row,column) = Printf.sprintf "(%d,%d)" row column
end
