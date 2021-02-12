(* Configuration of extension:
 * Should we geneate C++ code, should we add debug printing, etc. *)
type config =
  { mutable gencpp : bool
  ; mutable destdir : string
  ; mutable ext : string
  ; mutable insert_locks : bool
  ; mutable trace_locks : bool
  }

let config =
  { gencpp = true; destdir = "."; ext = "cpp"; insert_locks = true; trace_locks = false }
;;

exception ErrorMsg of string * Location.t

let () =
  Location.register_error_of_exn (fun exn ->
      match exn with
      | ErrorMsg (msg, loc) -> Some (Location.error ~loc msg)
      | _ -> None)
;;

let ppxqt_failed ~loc msg = raise @@ ErrorMsg (msg, loc)

open Ppxlib
open Base

let rec has_attr name : Parsetree.attributes -> bool = function
  | [] -> false
  | { attr_name = { txt; _ }; _ } :: _ when String.equal txt name -> true
  | _ :: xs -> has_attr name xs
;;

let find_attr_exn ~name xs =
  List.find_map_exn xs ~f:(fun { attr_name = { txt; _ }; attr_payload } ->
      if String.equal txt name then Some attr_payload else None)
;;

let find_attr ~name xs =
  List.find_map xs ~f:(fun { attr_name = { txt; _ }; attr_payload } ->
      Format.printf "checking %s and %s\n%!" txt name;
      if String.equal txt name then Some attr_payload else None)
;;
