type +'a obj

external meth_A_foo : [`qobject] obj -> unit = "call_A_foo"
external create_a : < .. > -> [`qobject ] obj = "create_AA"
    
class a = object (self)
  val mutable handler : [`qobject] obj = Obj.magic 1
  initializer handler <- create_a self 
  
  method foo () = 
      print_endline "calling a.foo";
      meth_A_foo handler;
      print_endline "end of calling a.foo"
end

class b = object (self) 
  inherit a as super
  method boo () = print_endline "b.boo"

end

