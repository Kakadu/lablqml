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



