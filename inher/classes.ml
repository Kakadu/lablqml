type +'a obj

external meth_A_foo : [`qobject] obj -> unit = "call_A_foo"
external set_caml_obj : [`qobject] obj -> < .. > -> unit = "set_caml_obj"
external create_a' : unit -> [`qobject ] obj = "create_AA"


class a me = object (self)
  initializer  set_caml_obj me self
  val handler : [`qobject] obj = me
  method foo () = 
        print_endline "calling a.foo";
        meth_A_foo me;
        print_endline "end of calling a.foo"
end

let create_a () = new a (create_a' ()) 

class b  = object (self) 
  inherit a (create_a' ()) as super
  method boo () = print_endline "b.boo"

end

