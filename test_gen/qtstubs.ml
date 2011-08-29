type 'a obj

let (|>) x f = f x 

class virtual ['a] sssignal = object 
  method virtual name : string
end
class virtual ['a, 'c] ssslot = object 
  method virtual name : string 
  method virtual call : 'c 
end

external make_root_widget : unit -> [`qobject] obj = "make_root_widget"

let wrap_handler = 
  fun funcname argname arg -> match arg with
    | Some o -> Some (o#handler )
    | None -> None (* Printf.printf "Error when calling #handler in %s for arg %s\n"
    funcname argname; assert false *)

module QApplication = struct
  type t = [`qapplication]
  external create : string array -> t obj = "ml_QApplication"
  external exec : [> `qapplication] obj -> int = "ml_QApplication_exec"
end
