open Core_kernel
open Core_kernel.Std
open ParseYaml
open Printf
open Helpers

let () = Printexc.record_backtrace true

type options = {
  mutable filename : string;
  mutable target : [ `Qml | `QtGui | `Qml_wrap | `List ];
  mutable add_debug_calls: bool;
  mutable debug_critical_sections: bool;
  mutable default_extension : [ `C | `CPP ];   (* Which extension to use for generated files *)
  mutable dest_dir : string;            (* Where to put generated files *)
}

let options =
  { filename = "input_yaml"
  ; target = `Qml_wrap
  ; add_debug_calls = false
  ; debug_critical_sections = false
  ; default_extension = `CPP
  ; dest_dir = "."
  }

let () =
  let specs = ref
    [ ("-qml",        Arg.Unit (fun () -> options.target <- `Qml),   "use qml")
    ; ("-qtgui",      Arg.Unit (fun () -> options.target <- `QtGui), "use QtGui")
    ; ("-qml_wrap",   Arg.Unit (fun () -> options.target <- `Qml_wrap), "use Qml_wrap")
    ; ("-list",       Arg.Unit (fun () -> options.target <- `List),     "print names of declared classes")
    ; ("-with-debug", Arg.Unit (fun () -> options.add_debug_calls <- true),
       "Add qDebug() calls in beginning of member functions")
    ; ("-with-debug-criticals", Arg.Unit (fun () -> options.debug_critical_sections <- true),
       "Add qDebug() when using critical sections")
    ; ("-destdir",    Arg.String (fun s -> options.dest_dir <- s), "Where to put generated files")
    ; ("-ext",        Arg.String (function | "c"   -> options.default_extension <- `C
					   | "cpp" -> options.default_extension <- `CPP
	                                   | ____  -> failwith "Unknown parameter of -ext"),
       "set extension for generated files")

    ; ("--help",      Arg.Unit (fun () -> ignore (Sys.command "man mocml")), "help")
    ]
  in
  let usage_msg = "Usage:" in
  specs := !specs @ [("-h", Arg.Unit (fun () -> Core_arg.usage !specs usage_msg; exit 0), "Display this list of options")];
  Core_arg.parse !specs
    (fun s -> options.filename <- s; print_endline ("Setting filename " ^ s))
    usage_msg

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
  | `List
  | `Qml_wrap as mode -> begin
    if not (Sys.file_exists options.filename) then (
      Printf.printf "File '%s' not found\n" options.filename;
      exit 1
    );
    let data =
      try ParseYaml.Json.parse_file options.filename
      with
      | exc ->
        begin match exc with
        | ParseYaml.Json.ParseError(msg,js) ->
          print_endline "In:";
          Yojson.Basic.pretty_to_channel stdout js;
          printf "\nParse error: %s\n%!" msg;
          exit 1
        | Yojson.Json_error msg ->
          print_endline msg;
          exit 1
        | _ -> print_endline (Exn.to_string exc)
        end;
        Printexc.print_backtrace Out_channel.stdout;
        exit 1
    in
    match mode with
    | `Qml_wrap ->
      let config =
        [`Ext options.default_extension] @
        (if options.add_debug_calls then [`PrintMethCalls] else []) @
        (if options.debug_critical_sections then [`DebugBlockingSections] else [])
      in
      List.iter data ~f:(Qml_wrap.generate ~directory:options.dest_dir ~config)
    | `List ->
      let open Yaml2.Types in
      data |> List.map ~f:(fun c -> c.classname) |> String.concat ~sep:" " |> print_endline
  end
