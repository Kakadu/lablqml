open Core
open Core.Std
open SuperIndex
open Parser
open Generators
open Printf

let constr_weight ~index lst =
  let arg_weight arg = match pattern index arg with
    | InvalidPattern -> assert false
    | ObjectDefaultPattern -> 1
    | ObjectPattern when Option.is_some arg.arg_default -> 1
    | PrimitivePattern _ when Option.is_some arg.arg_default -> 1
    | _ -> 0
  in
  List.fold ~f:(fun acc x -> acc + (arg_weight x)) ~init:0 lst

let eliminate_lst (lst: constr list) =
  let lst' = List.map lst
    ~f:(fun x -> (x,x |> List.map ~f:string_of_arg |> String.Set.of_list) ) in
  let rec helper lst = match lst with
  | [] | [_] -> lst
  | h::tl ->
    let init = [h] in
    let f acc ((_,x'') as x) =
      let (a,b) = Core_list.partition_tf acc ~f:(fun (_,y) -> String.Set.subset y x'') in
      x :: b
    in
    List.fold tl ~init ~f
  in
  helper lst' |> Core_list.map ~f:fst
;;

(*
let qstring_ref_type = { t_name="QString"; t_indirections=0; t_is_ref=true; t_params=[]; t_is_const = false }
let is_cstring_type t = t.t_name="char" && t.t_indirections = 1 ;;
*)

let filter_constrs index = SuperIndex.map ~f:(function
  | Enum e -> Enum e
  | Class (c,lst) ->
    let classname  = c.c_name in
    let constrs = List.filter c.c_constrs
      (fun c -> is_good_meth ~classname ~index (meth_of_constr ~classname c))  in
    let constrs =
      List.stable_sort constrs ~cmp:(fun b a -> compare (List.length a) (List.length b))
      |> List.rev in

    let constrs = eliminate_lst constrs in
    (* Dublicate constructors can exist
       QImage* QImage(const char* fileName , const char* format  = 0 )
       QImage* QImage(const QString & fileName , const char* format  = 0 )
    *)
    let constrs = List.stable_sort constrs ~cmp:(fun b a ->
      let (x,y) = (constr_weight ~index  a, constr_weight ~index b) in
      if x <> y then compare x y else begin
	let (s1,s2) = (Parser.string_of_constr ~classname:"S" a, string_of_constr ~classname:"S" b) in
	String.compare s1 s2
      end
    ) in
    let new_class = { c with c_constrs = constrs } in
    Class (new_class,lst)
) index
