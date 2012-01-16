open Core
open Parse
open Printf
open Helpers


let gen_header ~classname meths =
  let h = open_out (classname ^ ".h") in
  let big_name = String.capitalize classname ^ "_H" in
  fprintf h "#ifndef %s\n" big_name;
  fprintf h "#define %s\n" big_name;


  fprintf h "#endif\n\n";
  close_out h
