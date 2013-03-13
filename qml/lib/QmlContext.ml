type t

module M = Map.Make(String)

let container: t M.t ref = ref M.empty

let add_view name (v: t) =
  container := M.add name v !container
let () =
  print_endline "registering add_view";
  Callback.register "register_view" add_view;
  print_endline "registered"

let get_view_exn ~name = M.find name !container

let get_view ~name =
  try Some (get_view_exn name)
  with Not_found -> None


external set_context_property: ctx:t -> name:string -> 'a -> unit
  = "caml_setContextProperty"
