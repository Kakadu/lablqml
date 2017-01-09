This is an old idea about how to implement SIGNAL/SLOT connection. It is not in master and when we stickwith QML it is not so useful (read OUTDATED).

Now in OCaml there is no way to write some function `'a->'b->'a` where 'a is subtype of 'b.
That's why my type-safe connection is almost type-safe.

Let's create to classes to incapsulate signals and slots:

    class virtual ['a] sssignal = object 
      method virtual name : string
    end
    class virtual ['a, 'c] ssslot = object 
      method virtual name : string 
      method virtual call : 'c 
    end
`name` is then name of signal or slot in Qt. `call` is used to call slot.

    class a ... = object (self) 
      method signal1 = object (self : <arg1:int; arg2:bool; ..> #sssignal)
        method name = "signal1"
      end
      method signal2 = object (self : <arg1: int;  ..> #sssignal)
        method name = "signal2"
      end
    end
Class `a` has some signals

    class b ... = object (self)
      method slot1 = object (self : (< arg1:int; ..>, int->unit) #ssslot)
        method name = "slot1"
        method call = fun (_:int) -> ()
      end
      method slot2 = object (self: (<arg1: int; arg2: bool; ..> , int -> bool -> unit)  #ssslot)
        method name = "slot2"
        method call = fun (_:int) (_:bool) -> ()
      end
      method slot3 = object (self : (<..>, int) #ssslot)
        method name = "slot3"
        method call = 5
      end     
      method slot4 = object (self: ( <arg1: bool; arg2: int list; ..>,  bool->int list-> unit) #ssslot)
        method name= "slot4"
        method call = fun (_:bool) (_:int list) -> ()
      end
    end
Class `b` has some slots

    let connect: 'a 'b. 'a sssignal -> ('a, 'b) ssslot -> unit = ...
And function for creating connections.

    let aa = new a 5 and bb = new b 6
    let () = connect aa#signal1 bb#slot1
    let () = connect aa#signal1 bb#slot2
    let () = connect aa#signal1 bb#slot3
These lines are compilable



