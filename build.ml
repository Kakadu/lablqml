print_endline "Configure script for lablqt";;
#load "unix.cma";;
open UnixLabels;;
open Sys;;
open Printf;;
(* Configure part *)
let wrap_cmd cmd err = 
  let x = command cmd in
  if x<>0 then failwith err;;

wrap_cmd "pkg-config --cflags QtOpenGL" "can't find Qt OpenGL header files";;
(* Configure end ***************************)

let cores_count = 3;; (* make -j parameter *)

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

let touch s =
  if not (Sys.file_exists s) then 
        close_out (open_out s);;

touch "xml/.depend";;
print_endline "Now you can build xml generator using:";;
print_endline "\tcd xml; make depend; make\n";;

if not (file_exists "test_gen/out") then
        mkdir ~perm:0o777 "test_gen/out";;

if not (file_exists "test_gen/out/cpp") then
        mkdir ~perm:0o777 "test_gen/out/cpp";;

if not (file_exists "xml/out") then
        symlink ~src:"../test_gen/out" ~dst:"xml/out";;

(* compiling xml *)
chdir "xml";;
wrap_cmd "make clean" "error while compiling xml";;
wrap_cmd "make depend" "error while compiling xml";;
wrap_cmd "make all" "error while compiling xml";;
chdir "..";;
print_endline "Generator is compiled\n";;

print_endline "Building XML tool for simplifing API";;
wrap_cmd "make -C xmltool" "error while compiling xmltool";;

print_endline "executing xmltool";;
chdir "xmltool";;
wrap_cmd "./4test5" "Error while generating tests for test N5";;
chdir "..";;


print_endline "executing generator\n";;
chdir "xml";;
wrap_cmd (sprintf "./main.opt -xml %s -file %s" (cpp_includes ()) api_xml) "error while generating code";;
chdir "..";;

print_endline "\ncompiling generated C++ files\n";;
wrap_cmd (Printf.sprintf "make -j%d -C test_gen/out/cpp" cores_count) 
  "error while compiling generated C++ files";;

print_endline "\ncompiling mocml\n";;
touch "moc/.depend";;
wrap_cmd "make -C moc depend" "can't make depend on mocml";;
let () = wrap_cmd "make -C moc" "error while building mocml";;

let add_mocml where =
  let file = where ^ "/mocml" in 
  if not (file_exists file) then
    symlink ~src:"../../moc/main.opt" ~dst:file;;
List.iter add_mocml ["test_gen/test4";"test_gen/test5"; "test_gen/test6"];;

print_endline "\ncompiling the lablqt library";;
wrap_cmd "make -C test_gen clean all" "error while building library";;

print_endline "making tests";;
let tests = ["test";"test2";"test3";"test4"] in
List.iter (fun s -> wrap_cmd ("make -C test_gen/"^s) ("can't make test " ^ s)) tests;;

List.iter (fun test ->
  print_endline "\npreconfigure for ";
  print_endline (test ^ "\n");
  chdir (sprintf "test_gen/%s" test);
  touch ".depend";

  wrap_cmd "make depend" (sprintf "can't make %s (depend failed)" test);
  wrap_cmd "make"        (sprintf "can't make %s (make failed)" test);
  chdir "../.."
) ["test5";"test6"];;



