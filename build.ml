print_endline "Configure script for lablqt";;
#load "unix.cma";;
open UnixLabels;;
open Sys;;
open Printf;;

let cores_count = 3;; (* mkae -j parameter *)
let api_xml = "../for_test5.xml";; 
(* .. because this file will be accesses from ./xml *)

let touch s =
  if not (Sys.file_exists s) then 
        close_out (open_out s);;

let wrap_cmd cmd err = 
  let x = command cmd in
  if x<>0 then failwith err;;


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
wrap_cmd "make -C xml depend clean all" "error while compiling xml";;

(* executing generator *)
chdir "xml";;
wrap_cmd (sprintf "./main.opt -xml -file %s" api_xml) "error while generating code";;
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
List.iter add_mocml ["test_gen/test4";"test_gen/test5"];;

print_endline "\ncompiling the lablqt library";;
wrap_cmd "make -C test_gen clean all" "error while building library";;

print_endline "making tests";;
let tests = ["test";"test2";"test3";"test4"] in
List.iter (fun s -> wrap_cmd ("make -C test_gen/"^s) ("can't make test " ^ s)) tests;;


print_endline "\npreconfigure for test 5\n";;
let _ = command "rm -f test_gen/test5/.depend";;
touch "test_gen/test5/.depend";;
wrap_cmd "make -C test_gen/test5 depend" "can't make test5";;

print_endline "There are some problems with compiling test5 now";;
print_endline "You can do `cd test_gen/test5; mkae depend; make` manually";;
(*
wrap_cmd "make -C test_gen/test5 "       "can't make test5";;
*)

