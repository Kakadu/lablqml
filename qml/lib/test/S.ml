open Types
open Helpers

let files_in_dir path =
  let d = Unix.opendir path in
  let ans = ref [] in
  try
    while true do ans := (Unix.readdir d) :: !ans done;
    assert false
  with
      End_of_file -> Unix.closedir d; !ans

let modulename_of_file filename =
  let ans = String.copy filename in
  ans.[0] <- Char.uppercase ans.[0];
  ans

let process_cmi_file filename : Types.signature =
  let ic = open_in_bin filename in
  let magic_len = String.length (Config.cmi_magic_number) in
  let buffer = String.create magic_len in
  really_input ic buffer 0 magic_len ;
  let (name, sign) = input_value ic in
  close_in ic;
  sign


let read_modules dirs : Types.signature_item list =
  let files =
    List.map dirs ~f:(fun path ->
      files_in_dir path
      |> List.filter ~f:(fun s -> Filename.check_suffix s ".cmi")
      |> List.map ~f:(fun name -> (path,name))
    ) |> List.flatten in
  List.map (fun (path,filename) ->
    let module_name = modulename_of_file (Filename.chop_extension filename) in
    let sign = process_cmi_file (path ^/ filename) in
    Types.Sig_module (Ident.({name=module_name; flags=0;stamp=0}), Types.Mty_signature sign, Types.Trec_not)
  ) files


let build_tree (xs : Types.signature) =
  let open Tree in
  let name = Ident.({stamp=0; name="root"; flags=0}) in
  let internal = Types.Sig_module(name,Types.Mty_signature xs, Types.Trec_not) in
  (*let xs = if List.length xs > 10 then List.take ~n:10 xs else xs in*)
  let sons = List.map xs ~f:of_sig_item in
  {name="root"; internal; sons}

(*let sort = SValue | SType | SExn | SMod | SModType | SCls | SClsTyp*)
let sort_of_sig_item = function
  | Types.Sig_value       _ -> "v"
  | Types.Sig_type        _ -> "t"
  | Types.Sig_exception   _ -> "cn"
  | Types.Sig_module (_,Types.Mty_functor _,_) -> "f"
  | Types.Sig_module      _ -> "m"
  | Types.Sig_modtype     _ -> "mt"
  | Types.Sig_class       _ -> "c"
  | Types.Sig_class_type  _ -> "ct"
