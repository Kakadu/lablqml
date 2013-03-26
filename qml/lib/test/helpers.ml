open ListLabels
open Printf

let (^/) a b = sprintf "%s/%s" a b
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

module List = struct
  include ListLabels

  let make n ~f =
    let ans = ref [] in
    for i=n downto 0 do
      ans := (f i) :: !ans
    done;
     !ans

  let filter_map ~(f: 'a -> 'b option) (xs: 'a list) : 'b list =
    let f : 'b list -> 'a -> 'b list = fun acc x ->
      match f x with
        | Some y -> y::acc
        | None   -> acc in
    fold_right ~init:[] ~f xs

  let rec last = function
    | [] -> raise (Failure "bad argument [] of last")
    | [x] -> x
    | x::y::xs -> last (y::xs)

  let nth where ~n =
    if n<0 then raise (Failure "bad argument of nth")
    else nth where n

  let rec take ?(acc=[]) xs ~n =
    match xs with
    | _ when n = 0 -> rev acc
    | h::tl -> take ~acc:(h::acc) tl ~n:(n-1)
    | [] -> raise (Failure "List.take")

  let rec drop xs ~n =
    if n<0 then raise (Failure (sprintf "bad argument %d of drop" n)) else
    if n=0 then xs else
    match xs with
      | x::xs -> drop xs ~n:(n-1)
      | []    -> raise (Failure "List.drop")

  let rec iter_with_tail f = function
    | [] -> ()
    | h :: tl -> f h tl; iter_with_tail f tl

  let zip_non_exn xs ys =
    let rec helper acc (xs,ys) =
      match (xs,ys) with
      | (h1::t1,h2::t2) -> helper (fun l -> (h1,h2) :: (acc l)) (t1,t2)
      | ([],_)
      | (_,[]) -> acc
    in
    helper (fun _ -> []) (xs,ys) [] |> List.rev

  let take_while cond xs =
    let rec helper acc = function
      | [] -> List.rev acc
      | h::tl when cond h -> helper (h::acc) tl
      | _:: tl  -> helper acc tl
    in
    helper [] xs

  let to_string xs ~f =
    map ~f xs |> String.concat "," |> sprintf "[%s]"
end
