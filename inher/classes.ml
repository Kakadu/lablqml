type +'a obj

external meth_A_foo : [`qobject] obj -> int = "call_A_foo"

class a me = object
  val handler : [`qobject] obj = me
  method foo = meth_A_foo me
end



