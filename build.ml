#load "unix.cma";;

open UnixLabels

let () = print_endline "Configure script for lablqt"
let (^/) a b = a^"/"^b
let flush () = flush Pervasives.stdout

let target = ref `Build

let touch s = close_out (open_out s)

let () = Arg.parse [
        ("-build",     Arg.Unit (fun () -> target := `Build),     "");
        ("-clean",     Arg.Unit (fun () -> target := `Clean),     "");
        ("-install",   Arg.Unit (fun () -> target := `Install),   "");
        ("-uninstall", Arg.Unit (fun () -> target := `Uninstall), "");
] (fun _ -> ()) ""

(* Configure part *)

let wrap_cmd cmd err = 
  print_endline ("executing " ^ cmd);
  flush ();
  let x = Sys.command cmd in
  if x <> 0 then (print_endline ("command has returned " ^ string_of_int x) ; failwith err)


let cores_count = 3 (* make -j parameter *)

let make ?(dir = ".") ?j target err_msg = 
  let make_cmd = try Sys.getenv "MAKE" with Not_found -> "make" in
  let cmd = String.concat " " [
          make_cmd ; "-C" ; dir ;
          (match j with Some x -> Printf.sprintf "-j%d" x | None -> "");
          target] in
  wrap_cmd cmd ("error while " ^ err_msg)

(*
  match command cmd with
  | 0 -> ()
  | _ -> begin
    if Sys.file_exists (dir ^ "/.depend") then
      ignore (command (String.concat " " [make_cmd; "-C " ^ dir; "depend"]));
    ignore (command (String.concat " " [make_cmd; "-C " ^ dir; "clean"]));
    wrap_cmd cmd err_msg
  end
*)

(*
let () =
  wrap_cmd "pkg-config --cflags QtOpenGL" "can't find Qt OpenGL header files"
*)
(* Configure end ***************************)


(* This is a XML api file. generator will use it to take information about Qt
 * API. This file can be generated from bigger one using xml/xmltool.native 
 * executable. 
 * Now it is generated from `qt5-gui.xml` via ./qt5-minimal.sh script.
 * This file will not be changed while building.
 * *)
let api_xml = "qt5-minimal.xml"

let qt5 =
  try
    Unix.getenv "QT5"
  with Not_found ->
    Printf.eprintf "Cannot locate Qt5 libraries\n";
    Printf.eprintf "Please do `export QT5=....` to provide them\n";
    Printf.eprintf 
     "They will be use like $QT5/bin for moc and $QT5/lib for libraries\n";
    exit 1
;;
let includes = ["-I" ^ qt5 ^/ "include"]
let cpp_bin_loc = ref (qt5 ^/ "bin")

let cpp_includes () =
  match includes with
    | [] -> ""
    | _  -> " -I " ^ (String.concat " -I " includes);;

let () = match !target with 
  | `Build -> begin
        (* this checks the version of the ocaml running this script
           it should probably check the version of ocamlc instead
           but who cares *)

        if Sys.ocaml_version < "3.12" then
          failwith (Printf.sprintf "OCaml version 3.12 or higher is required (%s was provided)." Sys.ocaml_version);

        if not (Sys.file_exists "test_gen/out") then
                mkdir ~perm: 0o755 "test_gen/out";

        if not (Sys.file_exists "test_gen/out/cpp") then
                mkdir ~perm: 0o755 "test_gen/out/cpp";

        (* compiling xml *)

        make "all" ~dir: "src" "\ncompiling xml...\n";
        print_endline "\nGenerator is compiled!\n";

        (*print_endline "\nexecuting xmltool...\n";
        wrap_cmd "./for_test5.sh" "Error while generating tests for test N5";
*)
        print_endline "\nexecuting generator...\n";
        wrap_cmd 
          ("./src/generator.native -xml " ^ api_xml ^ 
           (if !cpp_bin_loc<>"" then " -qtloc " ^ !cpp_bin_loc else "") ^
           " " ^ (cpp_includes ()) ^ " test_gen/out")
          "error while generating code";

        make ~dir:"test_gen" "clean" "cleaning OCaml files";
        make ~dir:"test_gen" "helpers" "compiling helpers";

        print_endline "\ncompiling generated OCaml file...\n";
        make ~dir:"test_gen/out" ~j:1 "" "compiling generated OCaml code";

        print_endline "\ncompiling generated C++ files...\n";
        make ~dir:"test_gen/out/cpp" ~j:cores_count "" 
          "compiling generated C++ files";
        (*
        print_endline "\ncompiling mocml\n";
        make ~dir:"moc" "" "building mocml...";

        let add_mocml where =
          let file = where ^ "/mocml" in 
          if not (Sys.file_exists file) then
            symlink ~src: "../../moc/_build/main.native" ~dst: file
        in
        List.iter add_mocml ["test_gen/test4" ; "test_gen/test5" ; "test_gen/test6"];
        *)

        print_endline "\ncompiling the lablqt library...\n";
        make ~dir:"test_gen" "all" "building library";

        print_endline "\nmaking tests...\n";
        let tests = ["test"] in
        List.iter (fun s -> make ~dir: ("test_gen/"^s) "" ("compiling test " ^ s)) tests;

        List.iter (fun test ->
          print_endline "\npreconfigure for ";
          print_endline (test ^ "\n");
          chdir ("test_gen/" ^ test);
          touch ".depend";

          make "depend" ("computing dependencies for " ^ test);
          make ""       ("building " ^ test);
          chdir "../.."
        ) [] (* ["test5";"test6"] *)
  end
  | `Install -> 
    print_endline "installing...";
    let instcmd = "ocamlfind install lablqt test_gen/lablqt.cma test_gen/dlllablqt.so META " ^ 
      " test_gen/out/*.cmi test_gen/liblablqt.a test_gen/*.cmi test_gen/lablqt.a" ^
      " test_gen/lablqt.cmxa"  in
    wrap_cmd instcmd "can't do install"

  | `Uninstall -> 
    print_endline "uninstalling...";
    wrap_cmd "ocamlfind remove lablqt" "can't remove package"

  | `Clean -> 
    wrap_cmd "make -C src clean" "cleaning in sources";

    wrap_cmd "make -C test_gen clean" "cleaning in test_gen";
    wrap_cmd "rm -rf test_gen/out/*"  "removing generated files";

    (* I dont remove XML API file *)
    ()
