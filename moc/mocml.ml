open Core
open Parse
open Printf
open Helpers

type options = {
  mutable filename : string;
  mutable target : [ `Qml | `QtGui ]
}

let options = {filename = "input_yaml"; target = `Qml }

let () = Core_arg.parse 
  [ ("qml",   Core_arg.Unit (fun () -> options.target <- `Qml),  "use qml")
  ; ("qtgui", Core_arg.Unit (fun () -> options.target <- `QtGui),"use QtGui")
  ] (fun s -> options.filename <- s; 
    print_endline ("Setting filename " ^ s)
) "usage_msg"

let () = match options.target with
  | `QtGui -> begin
    let funcs = Parse.parse options.filename in
    let () = print_endline ("slots parsed: " ^ string_of_int (List.length funcs)) in
    let () = Qtgui.gen_header funcs in
    let () = Qtgui.gen_ml funcs in
    ()
  end
  | `Qml -> begin
    let data = Parse.Yaml2.parse_file options.filename in
    let () = print_endline "data file parsed" in
    let open Parse.Yaml2 in
    let () = data |> Types.sexp_of_data |> Sexplib.Sexp.to_string_hum |> print_endline in
    let () = List.iter data ~f:(fun c ->
      Qml.gen_header c;
      Qml.gen_cpp c
    ) in
    ()
  end

















