print_endline "Configure script for lablqt";;
#load "unix.cma";;
open UnixLabels;;
open Sys;;
open Printf;;

let target = ref `Build;;

let touch s =
(*  if not (Sys.file_exists s) then *)
        close_out (open_out s)
;;
open Arg;;
let () = Arg.parse [
        ("-build", Unit( fun () -> target := `Build ), "");
        ("-clean", Unit( fun () -> target := `Clean ), "");
        ("-install", Unit (fun () -> target := `Install), "");
        ("-uninstall", Unit (fun () -> target := `Uninstall), "")
] (fun _ -> ()) "";;

(* Configure part *)
let wrap_cmd cmd err = 
  let x = command cmd in
  if x<>0 then failwith err;;

let cores_count = 3;; (* make -j parameter *)

let make ?(dir=".") ?j target err_msg = 
  let make_cmd = try Sys.getenv "MAKE" with Not_found -> "make" in
  let cmd = String.concat " " [
          make_cmd; "-C " ^ dir; 
          (match j with Some x -> sprintf "-j%d" x | None -> "");
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
;;

wrap_cmd "pkg-config --cflags QtOpenGL" "can't find Qt OpenGL header files";;
(* Configure end ***************************)


(* let api_xml = "aaa.xml";; *)
let api_xml = "for_test5.xml";; 
(* 
 * This is a XML api file. Then biggest and more interesting file is
 * aaa.xml but building with it takes too long. You can simplify aaa.xml using 
 * for_test5.sh script.
 * *)

(* You can setup GCC include files specific for your system *)
let includes = [];;

let cpp_includes () = match includes with 
  | [] -> ""
  | _  -> " -I " ^ (String.concat " -I " includes );;

let () = match !target with 
  | `Build -> begin
        if not (file_exists "test_gen/out") then
                mkdir ~perm:0o755 "test_gen/out";

        if not (file_exists "test_gen/out/cpp") then
                mkdir ~perm:0o755 "test_gen/out/cpp";

        (* compiling xml *)
        make "" ~dir:"xml" "compiling xml";
        print_endline "Generator is compiled\n";

        print_endline "executing xmltool";
        wrap_cmd "./for_test5.sh" "Error while generating tests for test N5";

        print_endline "executing generator\n";
        wrap_cmd (sprintf "./xml/main.native -xml %s %s test_gen/out" api_xml (cpp_includes ()))
          "error while generating code";

        print_endline "\ncompiling generated C++ files\n";
        make ~dir:"test_gen/out/cpp" ~j:cores_count "" 
          "compiling generated C++ files";

        print_endline "\ncompiling mocml\n";

        make ~dir:"moc" ""       "building mocml";

        let add_mocml where =
          let file = where ^ "/mocml" in 
          if not (file_exists file) then
            symlink ~src:"../../moc/_build/main.native" ~dst:file
        in
        List.iter add_mocml ["test_gen/test4";"test_gen/test5"; "test_gen/test6"];

        print_endline "\ncompiling the lablqt library";
        make ~dir:"test_gen" "all" "building library";

        print_endline "making tests";
        let tests = ["test";"test2";"test3";"test4"] in
        List.iter (fun s -> make ~dir:("test_gen/"^s)  "" ("compiling test " ^ s)) tests;

        List.iter (fun test ->
          print_endline "\npreconfigure for ";
          print_endline (test ^ "\n");
          chdir (sprintf "test_gen/%s" test);
          touch ".depend";

          make "depend" (sprintf "computing dependencies for %s" test);
          make ""       (sprintf "building %s"   test);
          chdir "../.."
        ) ["test5";"test6"]
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
    wrap_cmd "make -C xml clean" "cleaning in xml";

    wrap_cmd "make -C test_gen clean" "cleaning in test_gen";
    wrap_cmd "rm -rf test_gen/out/*"  "removing generated files";

    wrap_cmd "make -C moc clean" "cleaning in moc";

    (* I dont remove XML API file **)
    ()
;;
