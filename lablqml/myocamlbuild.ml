open Printf
open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support.
 * Adopted from tsdl's myocamlbuild.ml
 *)

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"
let caml_stdlib =
  let out = Ocamlbuild_pack.My_unix.run_and_read "ocamlfind c -where" in
  try Scanf.sscanf out "%s\n" (fun x -> x)
  with End_of_file -> failwith (sprintf "%s: can't get ocaml library path" __FILE__)

let cxx = "g++"

let pkg_config flags package =
  let cmd tmp =
    Command.execute ~quiet:true &
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd

let make_opt o arg = S [ A o; arg ]
let make_ccopts xs =
  List.concat (List.map (fun x -> [A"-ccopt";A x]) xs)

let pkg_config_lib ~lib (*~has_lib ~stublib *) =
  let cflags = (*(A has_lib) :: *) pkg_config "cflags" lib in (*
  let stub_l = [A (Printf.sprintf "-l%s" stublib)] in *)
  let cflags = A("-I" ^ caml_stdlib) :: cflags in
  let libs_l = pkg_config "libs-only-l" lib in
  let libs_L = pkg_config "libs-only-L" lib in
  let linker = match os with
  | "Linux\n" -> [A "-Wl,-no-as-needed"]
  | _ -> []
  in
  let mklib_flags = (List.map (make_opt "-ldopt") linker) @ libs_l @ libs_L in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") libs_l in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in (*
  let stublib_flags = List.map (make_opt "-dllib") stub_l  in *) (*
  let tag = Printf.sprintf "use_%s" lib in *)
  flag ["c"; "ocamlmklib"; "use_qt5"] (S mklib_flags);

  List.iter (fun tag ->
    flag ["c"; "compile"; tag] (S compile_flags);
    (* Some tricks required for copying .h file to builddir. I use poor man solution
       for now. See link for right solution
    https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc#dynamic-dependencies
    *)
    flag ["c"; "compile"; tag]
         (S (make_ccopts ["-std=c++11";"-fPIC";"-I.."] @ [A"-cc";A cxx]) );
    flag ["c"; "compile"; tag] (S [A"-ccopt";A"-Dprotected=public"]);
    (* flag ["c"; "compile"; tag] (S [A"-package";A"lablqml"]); *)
    flag ["c"; "compile"; tag] (S [A"-ccopt";A"-I."]);
  ) [ "mocml_generated"; "qtmoc_generated"; "qt_resource_file"
    ; "oasis_library_lablqml_ccopt"];

  flag ["c"; "compile"; "d_private_public"] (S [A"-ccopt";A"-Dprivate=public"]);

  flag ["link"; "ocaml"; "use_qt5"] (S (link_flags @ lib_flags));
  flag ["link"; "ocaml"; "use_qt5"] (make_opt "-cclib" (A"-lstdc++"));
  ()

(*
let resource_depends =
  [ "ui/ApiBrowser.qml"; "ui/main.js";"ui/PathEditor.qml";"ui/Root.qml";"ui/Scrollable.qml"
  ; "ui/ScrollBar.qml"
  (* pictures *)
  ; "ui/pics/minus-sign.png"; "ui/pics/plus-sign.png"
  ]
*)
let make_gcc_rules () =
  (* dependencies for c files from *)
  (* http://ocaml.org/learn/tutorials/ocamlbuild/Compiling_C_with_gcc.html *)
  let parallel files = List.map (fun f -> [f]) files in
  let err_circular file path =
    Printf.sprintf "Circular build detected (%s already seen in [%s])"
      file (String.concat "; " path)
  in
  let parse_deps file =
    printf "parse_deps %s\n%!" file;
    let dir = Pathname.dirname file in
    let deps = List.tl (List.tl (string_list_of_file file)) in
    let deps = List.filter (fun d -> d <> "\\") deps in (* remove \ *)
    let correct d = if Pathname.dirname d = dir then d else dir / d in
    List.map correct deps
  in
  let deps_action dep prod env build =
    let c = env dep in
    let tags = tags_of_pathname c in
    Cmd (S [A cxx; T tags; A "-MM"; A "-MG"; A "-MF"; Px (env prod); P c])
  in
  rule "c++: c -> c.depends"
    ~dep: "%.c"
    ~prod:"%.c.depends" (deps_action "%.c" "%.c.depends");

  rule "c++: h -> h.depends"
    ~dep:"%.h"
    ~prod:"%.h.depends" (deps_action "%.h" "%.h.depends");
  rule "c++: c & c.depends -> o"
    ~deps:["%.c"; "%.c.depends"]
    ~prod: "%.o"
    begin fun env build ->
      let c = env "%.c" in
      let rec build_transitive_deps = function
      | [] -> ()
      | (_, []) :: todo -> build_transitive_deps todo
      | (path, f :: rest) :: todo ->
          if List.mem f path then failwith (err_circular f path)
          else
            let deps = parse_deps (f ^ ".depends") in
            let dep_files = List.map (fun d -> d ^ ".depends") deps in
            List.iter print_endline dep_files;
            List.iter Outcome.ignore_good (build (parallel deps));
            List.iter Outcome.ignore_good (build (parallel dep_files));
            build_transitive_deps (((f :: path), deps) :: (path, rest) :: todo)
      in
      print_endline "A";
      build_transitive_deps [([],[c])];
      Cmd (S [A cxx;
          A "-Wall"; A "-Os"; A "-std=c99";
          T (tags_of_pathname c ++ "compile" ++ "avr-gcc");
          A "-c"; P c;
          A "-o"; Px (env "%.o");])
    end

let () =
  dispatch begin function
  | After_rules ->
    rule "Qt_moc: %.h -> moc_%.c"
      (* %(path) magic is useful when sources are located not in root directory *)
      ~prods:["%(path:<**/>)moc_%(modname:<*>).c"]
      ~dep:"%(path)%(modname).h"
      (begin fun env build ->
        tag_file (env "%(path)%(modname).h") ["qtmoc"];
        Cmd (S [A "moc"; P (env "%(path)%(modname).h"); Sh ">"; P (env "%(path)moc_%(modname).c")]);
       end);


    (* flag ["compile"; "oasis_library_lablqml_ccopt"] *)
    (*   (S[A"-ccopt"; A("-I"^ (F.ocaml_stdlib())) ]); *)

    (* rule "Qt resource: %.qrc -> qrc_%.c" *)
    (*   ~prods:["%(path:<**/>)qrc_%(modname:<*>).c"] *)
    (*   ~deps:("%(path)%(modname).qrc" :: resource_depends) *)
    (*   (begin fun env _ -> *)
    (*     (\* *)
    (*     tag_file (env "%(path)%(modname).h") ["qt_resource"]; *\) *)
    (*     Cmd(S[ A"rcc"; A"-name"; A(env "%(modname)"); P (env "%(path)%(modname).qrc") *)
    (*          ; A "-o"; P (env "%(path)qrc_%(modname).c")]) *)
    (*    end); *)

    pkg_config_lib ~lib:"Qt5Quick Qt5Widgets";
    (* flag ["ocaml"; "compile"; "use_ppx_qt"] (S[A"-ppx"; A"ppx_qt -nocpp"; A"-dsource"]); *)
    dep ["link"; "ocaml"; "use_qrc_stub"] ["src/qrc_resources.o"];
    flag ["link"; "ocaml"; "native"; "use_liblablqml_stubs" ]
      (S[A"-cclib"; A"-llablqml_stubs"]);
    flag ["link"; "ocaml"; "byte";   "use_liblablqml_stubs" ]
      (S[A"-cclib"; A"-llablqml_stubs"]);
    (* below are headers files generated by mocml
     * TODO: how to move them to _tags?               *)
    (* dep ["compile"; "c"] [ "src/dataItem.h" *)
    (*                      ; "src/controller.h" *)
    (*                      ; "src/historyModel.h" *)
    (*                      ; "src/abstractModel.h"]; *)

    (* Explicit dependenices for header. It will be great if ocamlbiuld could detect them
       himself *)
    (* dep ["compile"; "qsinglefunc"] ["QSingleFunc.h"]; *)
    (* dep ["compile"; "camlpropmap"] ["CamlPropertyMap.h"]; *)

    (* Some stuff for tests *)
    flag ["link"; "ocaml"; "native";   "use_lablqml" ]
      (S[A"-g"
        (* ;A"lablqml.cma" *)
        (* ;A"-cclib"; A"-llablqml_stubs" *)
        ;A"liblablqml_stubs.a"
        ]);

    make_gcc_rules ();
    ()
  | _ -> ()
  end
