open Core
open Core.Std
open ParseYaml
open Printf
open Helpers

let () = Printexc.record_backtrace true

type options = {
  mutable filename : string;
  mutable target : [ `Qml | `QtGui | `Qml_wrap ];
  mutable add_debug_calls: bool
}

let options = {filename = "input_yaml"; target = `Qml_wrap; add_debug_calls = false }

let () = Core_arg.parse
  [ ("-qml",        Arg.Unit (fun () -> options.target <- `Qml),   "use qml")
  ; ("-qtgui",      Arg.Unit (fun () -> options.target <- `QtGui), "use QtGui")
  ; ("-qml_wrap",   Arg.Unit (fun () -> options.target <- `Qml_wrap), "use Qml_wrap")
  ; ("-with-debug", Arg.Unit (fun () -> options.add_debug_calls <- true),
         "Add qDebug() calls in beginning of member functions")
  ; ("-help",       Arg.Unit (fun () -> ignore (Sys.command "man mocml")), "help")
  ; ("-h",          Arg.Unit (fun () -> ignore (Sys.command "man mocml")), "help")
  ] (fun s -> options.filename <- s;
    print_endline ("Setting filename " ^ s)
) "usage_msg"

let () = match options.target with
  | `QtGui -> begin
    let funcs = ParseYaml.parse options.filename in
    let () = print_endline ("slots parsed: " ^ string_of_int (List.length funcs)) in
    let slots = List.map funcs ~f:(fun (name,args) ->
      let args = Core_list.map args ~f:(fun x -> `Simple x) in
      (* FIXME check for lists too *)
      let res = Core_list.last_exn args in
      let body = Core_list.rev args |> Core_list.tl_exn in
      (name,body,res)
    ) in
    (*
    let clas =
      let open Parse.Yaml2.Types in
      ({classname="UserSlots"; slots; signals=[];members=[];props=[]}) in *)
    let () = Qtgui.gen_cpp slots in (* FIXME by passing clas.slots *)
    let () = Qtgui.gen_ml slots in
    ()
  end
  | `Qml -> begin
    let () = print_endline "data file parsed" in
    let data = ParseYaml.Json.parse_file options.filename in
    let open ParseYaml.Yaml2 in
    let () = data |> Types.sexp_of_data |> Sexplib.Sexp.to_string_hum |> print_endline in
    let () = List.iter data ~f:Qml2.gen_cpp in
    let () = List.iter data ~f:Qml2.gen_ml in
    ()
  end
  | `Qml_wrap -> begin
    if Core_sys.file_exists options.filename <> `Yes then (
      Printf.printf "File '%s' not found\n" options.filename;
      exit 1
    );
    let data =
      try  ParseYaml.Json.parse_file options.filename
      with exc ->
        Printf.printf "exception: %s\n" (Exn.to_string exc);
        Printexc.print_backtrace Out_channel.stdout;
        exit 1
    in
    let config =
      (if options.add_debug_calls then [`PrintMethCalls] else [])
    in
    List.iter data ~f:(Qml_wrap.generate ~config)
  end
