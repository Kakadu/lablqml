open Core
open Parse
open Printf
open Helpers

type options = {
  mutable filename : string;
  mutable target : [ `Qml | `QtGui ]
}

let options = {filename = "input"; target = `Qml }

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
    let data = Parse.parse_yaml options.filename in
    let () = print_endline "data file parsed" in
    let ans = data |> sexp_of_api_content |> Sexplib.Sexp.to_string_hum in
    let () = print_endline ans in
    List.iter data ~f:(fun (classname,meths) ->  Qml.gen_header ~classname meths);
    ()
  end


