open Printf
open Ocamlbuild_plugin
open Ocamlbuild_pack
open Command

module String = My_std.String
(* Generic pkg-config(1) support.
 * Adopted from tsdl's myocamlbuild.ml
 *)

let os = My_unix.run_and_read "uname -s" |> String.trim
let caml_stdlib =
  let out = My_unix.run_and_read "ocamlfind c -where" |> String.trim in
  try Scanf.sscanf out "%s" (fun x -> x)
  with End_of_file -> failwith (sprintf "%s: can't get ocaml library path" __FILE__)

let pkg_config flags package =
  let cmd tmp =
    Command.execute ~quiet:true &
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd

let cxx_compiler,cxx_flags,toolchain_opts =
  match Sys.getenv "LABLQML_ANDROID" with
  | _ ->
    (* let _ = failwith "" in *)
    let opam_prefix = My_unix.run_and_read "opam config var prefix" |> String.trim in
    let ndk_dir = opam_prefix ^ "/android-ndk" in
    let cxx = ndk_dir ^ "/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-g++" in
    (cxx,
      [ sprintf "-I%s/%s" ndk_dir "sources/cxx-stl/gnu-libstdc++/4.9/include"
      ; sprintf "-I%s/%s" ndk_dir "sources/cxx-stl/gnu-libstdc++/4.9/libs/armeabi-v7a/include"
      ; sprintf "-I%s/%s" ndk_dir "platforms/android-21/arch-arm/usr/include"
      ],
      ["-toolchain"; "android"])
  (* | _ -> ("g++", [],[]) *)
  | exception Not_found -> ("g++", [],[])

let cxx_flags = cxx_flags @
  ["-Wall";"-std=c++11";"-fPIC"]



let make_opt o arg = S [ A o; arg ]
let make_ccopts xs =
  List.concat (List.map (fun x -> [A"-ccopt";A x]) xs)

let pkg_config_lib ~lib (*~has_lib ~stublib *) =
  let cflags = (*(A has_lib) :: *) pkg_config "cflags" lib in (*
  let stub_l = [A (Printf.sprintf "-l%s" stublib)] in *)
  let cflags = [A("-I"^caml_stdlib); A"-Istubs"] @ cflags in
  let libs_l = pkg_config "libs-only-l" lib in
  let libs_L = pkg_config "libs-only-L" lib in
  let linker = match os with
  | "Linux" -> [A "-Wl,-no-as-needed"]
  | _ -> []
  in
  let mklib_flags = (List.map (make_opt "-ldopt") linker) @ libs_l @ libs_L in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") libs_l in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in (*
  let stublib_flags = List.map (make_opt "-dllib") stub_l  in *) (*
  let tag = Printf.sprintf "use_%s" lib in *)
  flag ["c"; "ocamlmklib"; "use_qt5"] (S mklib_flags);

  (* flag ["compile"] (S (List.map (fun x -> A x) toolchain_opts));
  flag ["c"] (S (List.map (fun x -> A x) toolchain_opts)); *)
  (* flag ["ocamlmklib"] (S (List.map (fun x -> A x) toolchain_opts)); *)

  List.iter (fun tag ->
    flag ["c"; "compile"; tag] (S compile_flags);
    (* Some tricks required for copying .h file to builddir. I use poor man solution
       for now. See link for right solution
    https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc#dynamic-dependencies
    *)
    flag ["c"; "compile"; tag]
         (S (make_ccopts cxx_flags @ [A"-cc";A cxx_compiler]) );
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
let () =
  dispatch begin function
  | After_rules ->
    rule "Qt_moc: %.h -> moc_%.c"
      (* %(path) magic is useful when sources are located not in root directory *)
      ~prods:["%(path:<stubs/>)moc_%(modname:<*>).c"]
      ~dep:"%(path)%(modname).h"
      (begin fun env build ->
        tag_file (env "%(path)%(modname).h") ["qtmoc"];
        Cmd (S [A "moc"; P (env "%(path)%(modname).h"); Sh ">"; P (env "%(path)moc_%(modname).c")]);
       end);

    pkg_config_lib ~lib:"Qt5Quick Qt5Widgets";
    flag ["link"; "ocaml"; "use_liblablqml_stubs" ] (S[A"-cclib"; A"-llablqml_stubs"]);
    flag ["link"; "ocaml"; "native"; "use_lablqml"] (S[A"liblablqml_stubs.a"]);


    (* rule "Qt resource: %.qrc -> qrc_%.c" *)
    (*   ~prods:["%(path:<**/>)qrc_%(modname:<*>).c"] *)
    (*   ~deps:("%(path)%(modname).qrc" :: resource_depends) *)
    (*   (begin fun env _ -> *)
    (*     (\* *)
    (*     tag_file (env "%(path)%(modname).h") ["qt_resource"]; *\) *)
    (*     Cmd(S[ A"rcc"; A"-name"; A(env "%(modname)"); P (env "%(path)%(modname).qrc") *)
    (*          ; A "-o"; P (env "%(path)qrc_%(modname).c")]) *)
    (*    end); *)

    (* flag ["ocaml"; "compile"; "use_ppx_qt"] (S[A"-ppx"; A"ppx_qt -nocpp"; A"-dsource"]); *)
    (* dep  ["link"; "ocaml"; "use_qrc_stub"] ["src/qrc_resources.o"]; *)

    (* below are headers files generated by mocml
     * TODO: how to move them to _tags?               *)
    (* dep ["compile"; "c"] [ "src/dataItem.h" *)
    (*                      ; "src/controller.h" *)
    (*                      ; "src/historyModel.h" *)
    (*                      ; "src/abstractModel.h"]; *)

    (* Explicit dependenices for header. It will be great if ocamlbiuld could detect them
       himself *)
    dep ["compile"; "c"] ["stubs/lablqml.h"];
    dep ["compile"; "qsinglefunc"] ["stubs/QSingleFunc.h"];
    dep ["compile"; "camlpropmap"] ["stubs/CamlPropertyMap.h"];

    (* Some stuff for tests *)
    ()
  | _ -> ()
  end
