open Core.Std
open Simplexmlparser
open Printf 
external (|>): 'a -> ('a -> 'b) -> 'b  = "%revapply"

let name  = function Element(x,_,_) -> x | PCData _ -> assert false
let attrs = function Element(_,x,_) -> x | PCData _ -> assert false
let sons  = function Element(_,_,x) -> x | PCData _ -> assert false
let find_node_exn ~name xs = List.find_exn xs ~f:(function Element(s,_,_) -> (s=name) | _ -> false)
let get_attr_exn ~name xs  = List.Assoc.find_exn xs name

let protectedConstructors  = function
  | Element(name,attr,ch) -> begin 
    let newSons = List.filter_map ch ~f:(function
      | Element("constructor",attr,sons) -> begin
        let newSons = List.filter_map sons ~f:(function
          | (Element ("accessPolicy",att,_)) as e -> 
              let v = get_attr_exn ~name:"value" att in
              if (v="private" || v="protected") then None else Some e
          | x -> Some x
        ) in
        Some (Element("constructor",attr,newSons) )
      end
      | x -> Some x
    ) in
    Some (Element(name,attr,newSons) )
  end  
  | x -> Some x

let noPrivateNonAbstract = function
  | Element(name,attr,ch) -> begin 
    let newSons = List.filter_map ch ~f:(function
      | Element("function",attr,children) -> begin
        let is_abstract = 
          let modif_node = find_node_exn ~name:"modifiers" children in
          try let _ = find_node_exn ~name:"virtual" (sons modif_node) in
              true
          with Not_found -> false
        in
        let new_children = List.filter_map children ~f:(function
          | (Element ("accessPolicy",att,_)) as e -> 
              let v = get_attr_exn ~name:"value" att in
              if ((v="private" || v="protected") && not is_abstract) then None else Some e
          | x -> Some x
        ) in
        Some (Element("function",attr,new_children) )
      end
      | x -> Some x
    ) in
    Some (Element(name,attr,newSons) )
  end  
  | x -> Some x



let noDestructors = function
  | Element(n,attr,ch) ->
      let new_children = List.filter ch ~f:(fun node -> (name node)<>"destructor") in
      Some( Element(n,attr,new_children) )
  | x -> Some x

let noMETAfuncs = function
  | Element(n,a,ch) ->
      let new_ch = List.filter ch  ~f:(function
        | Element ("function",a2,_) -> (get_attr_exn ~name:"name" a2 <> "d_func")
        | x -> true) in
      Some( Element(n,a,new_ch) )
  | x -> Some x

















