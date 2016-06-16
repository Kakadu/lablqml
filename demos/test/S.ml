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
  let open Bytes in
  let ans = of_string filename |> copy in
  set ans 0 (Char.uppercase ans.[0]);
  to_string ans

let process_cmi_file filename : Types.signature =
  let ic = open_in_bin filename in
  let magic_len = String.length (Config.cmi_magic_number) in
  let buffer = Bytes.create magic_len in
  really_input ic buffer 0 magic_len ;
  let (name, sign) = input_value ic in
  close_in ic;
  sign

module Types_helpers = struct
  let make_mod_struct ~name:name_ children =
    Types.Sig_module (Ident.({name=name_; flags=0;stamp=0}),
                      {md_type=Types.Mty_signature children; md_attributes=[]; md_loc=Location.none},
                      Types.Trec_not)
end

let read_modules dirs : Types.signature_item list =
  let files =
    List.map dirs ~f:(fun path ->
      files_in_dir path
      |> List.filter ~f:(fun s -> Filename.check_suffix s ".cmi")
      |> List.map ~f:(fun name -> (path,name))
    ) |> List.flatten in
  List.map ~f:(fun (path,filename) ->
    let module_name = modulename_of_file (Filename.chop_extension filename) in
    let sign = process_cmi_file (path ^/ filename) in
    Types_helpers.make_mod_struct ~name:module_name sign
  ) files


let build_tree (xs : Types.signature) =
  let open Tree in
  let internal = Types_helpers.make_mod_struct ~name:"root" xs in
  (*let xs = if List.length xs > 10 then List.take ~n:10 xs else xs in*)
  let sons = List.map xs ~f:of_sig_item in
  {name="root"; internal; sons}

let compare a b =
  let open Types in
  let helper = function
  | Types.Sig_type        _ -> 0
  | Types.Sig_modtype     _ -> 1
  | Types.Sig_module (_,{md_type=Types.Mty_functor _; _},_) -> 3
  | Types.Sig_module      _ -> 2
  | Types.Sig_class       _ -> 6
  | Types.Sig_class_type  _ -> 7
  | Types.Sig_value       _ -> 8
  | Types.Sig_typext      _ -> 9
  in
  let x = compare (helper a) (helper b) in
  if x<>0 then x
  else compare (Tree.name_of_item a) (Tree.name_of_item b)

let rec sort_tree ({Tree.sons;_} as item) =
  let open Tree in
  let cmp a b = compare a.internal b.internal in
  {item with sons = sons |> List.sort ~cmp |> List.map ~f:sort_tree}

let sort_of_sig_item = function
  | Types.Sig_value       _ -> "v"
  | Types.Sig_type        _ -> "t"
  | Types.Sig_typext      _ -> "ext"
  | Types.Sig_module (_,{md_type=Types.Mty_functor _;_},_) -> "f"
  | Types.Sig_module      _ -> "m"
  | Types.Sig_modtype     _ -> "mt"
  | Types.Sig_class       _ -> "c"
  | Types.Sig_class_type  _ -> "ct"
