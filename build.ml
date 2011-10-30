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

let make ?(dir=".") ?j cmd err_msg = 
  let make_cmd = "make" in
  let cmd = String.concat " " [
          make_cmd; "-C " ^ dir; 
          match j with Some x -> sprintf "-j%d" x | None -> "";
          cmd] in
  match command cmd with
  | 0 -> ()
  | _ -> begin
    if Sys.file_exists (dir ^ "/.depend") then
      ignore (command (String.concat " " [make_cmd; "-C " ^ dir; "depend"]));
    ignore (command (String.concat " " [make_cmd; "-C " ^ dir; "clean"]));
    wrap_cmd cmd err_msg
  end
;;

wrap_cmd "pkg-config --cflags QtOpenGL" "can't find Qt OpenGL header files";;
(* Configure end ***************************)


(* let api_xml = "../aaa.xml";; *)
let api_xml = "../for_test5.xml";; 
(* .. because this file will be accesses from ./xml 
 *
 * This is a XML api file. Then most biggest and most interesting file is
 * aaa.xml but Building with it is too long. You can simplify aaa.xml using 
 * xmltool/4test5 script.
 * *)

(* You can setup GCC include files specific for your system *)
let includes = [];;

let cpp_includes () = match includes with 
  | [] -> ""
  | _  -> " -I " ^ (String.concat " -I " includes );;

let () = match !target with 
  | `Build -> begin
        touch "xml/.depend";
        print_endline "Now you can build xml generator using:";
        print_endline "\tcd xml; make depend; make\n";

        if not (file_exists "test_gen/out") then
                mkdir ~perm:0o777 "test_gen/out";

        if not (file_exists "test_gen/out/cpp") then
                mkdir ~perm:0o777 "test_gen/out/cpp";

        if not (file_exists "xml/out") then
                symlink ~src:"../test_gen/out" ~dst:"xml/out";

        (* compiling xml *)
        chdir "xml";
        make "" "error while compiling xml";
        chdir "..";
        print_endline "Generator is compiled\n";

        print_endline "Building XML tool for simplifing API";
        make ~dir:"xmltool" "" "error while compiling xmltool";

        print_endline "executing xmltool";
        chdir "xmltool";
        wrap_cmd "./4test5" "Error while generating tests for test N5";
        chdir "..";

        print_endline "executing generator\n";
        chdir "xml";
        wrap_cmd (sprintf "./main.native -xml %s -file %s" (cpp_includes ()) api_xml) 
          "error while generating code";
        chdir "..";

        print_endline "\ncompiling generated C++ files\n";
        make ~dir:"test_gen/out/cpp" ~j:cores_count "" 
          "error while compiling generated C++ files";

        print_endline "\ncompiling mocml\n";
        touch "moc/.depend";
        
        make ~dir:"moc" "depend" "can't make depend on mocml";
        make ~dir:"moc" ""       "error while building mocml";

        let add_mocml where =
          let file = where ^ "/mocml" in 
          if not (file_exists file) then
            symlink ~src:"../../moc/main.opt" ~dst:file
        in    
        List.iter add_mocml ["test_gen/test4";"test_gen/test5"; "test_gen/test6"];

        print_endline "\ncompiling the lablqt library";
        make ~dir:"test_gen" "all" "error while building library";

        print_endline "making tests";
        let tests = ["test";"test2";"test3";"test4"] in
        List.iter (fun s -> make ~dir:("test_gen/"^s)  "" ("can't make test " ^ s)) tests;

        List.iter (fun test ->
          print_endline "\npreconfigure for ";
          print_endline (test ^ "\n");
          chdir (sprintf "test_gen/%s" test);
          touch ".depend";

          make "depend" (sprintf "can't make %s (depend failed)" test);
          make ""       (sprintf "can't make %s (make failed)"   test);
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
    List.iter (fun s -> 
      wrap_cmd (sprintf "rm -f %s" s) (sprintf "Can't remove %s" s);
      touch s
    ) ["xmltool/.depend"; "xml/.depend"; "moc/.depend"];

    wrap_cmd "make -C xml clean" "error while cleaning in xml";
    
    wrap_cmd "make -C xmltool clean" "error while cleaning in xml_tool";

    
    wrap_cmd "make -C test_gen clean" "error while cleaning in test_gen";
    wrap_cmd "rm -rf test_gen/out/*"  "error while removing generated files";

    wrap_cmd "make -C moc clean" "error while cleaning in moc";

    (* I dont remove XML API file **)
    ()
;;  
