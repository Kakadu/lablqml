open Core
open Parse
open Printf
open Helpers

let () = Printexc.record_backtrace true

type options = {
  mutable filename : string;
  mutable target : [ `Qml | `QtGui ]
}

let options = {filename = "input_yaml"; target = `Qml }

let () = Core_arg.parse 
  [ ("-qml",   Core_arg.Unit (fun () -> options.target <- `Qml),  "use qml")
  ; ("-qtgui", Core_arg.Unit (fun () -> options.target <- `QtGui),"use QtGui")
  ] (fun s -> options.filename <- s; 
    print_endline ("Setting filename " ^ s)
) "usage_msg"

let () = match options.target with 
  | `QtGui -> begin 
    let funcs = Parse.parse options.filename in
    let () = print_endline ("slots parsed: " ^ string_of_int (List.length funcs)) in
    let slots = Core_list.map funcs ~f:(fun (name,args) ->
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
    let data = Parse.Yaml2.parse_file options.filename in
    let () = print_endline "data file parsed" in
    let open Parse.Yaml2 in
    let () = data |> Types.sexp_of_data |> Sexplib.Sexp.to_string_hum |> print_endline in
    let () = List.iter data ~f:Qml.gen_cpp in
    ()
  end

















