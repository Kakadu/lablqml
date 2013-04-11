type 'a obj

let (|>) x f = f x 

class virtual ['a] sssignal = object 
  method virtual name : string
end
class virtual ['a, 'c] ssslot = object 
  method virtual name : string 
  method virtual call : 'c 
end

let wrap_handler = 
  fun funcname argname arg -> match arg with
    | Some o -> Some (o#handler )
    | None -> None 

external connect': [`qobject] obj -> signal:string -> dst:[`qobject] obj -> slot:string -> bool
    = "ml_QObject_connect"

let connect: 'a 'b. 'c -> 'a sssignal -> 'd -> ('a, 'b) ssslot -> unit = 
  fun sender si receiver sl -> (
        Printf.printf "Connecting %s -> %s\n" si#name sl#name;
        let (_:bool) = connect' sender#handler si#name receiver#handler sl#name in
        ()
  )


module QVariant = struct
    type t
end
module QModelIndex = struct
    type t
end

