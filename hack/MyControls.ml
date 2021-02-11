module MySingleton : sig
  (* [%property int someProperty READ somePropery WRITE setSomeProperty NOTIFY somePropertyChanged] *)
  (* [%qml_named_element MyApi] *)
  val doSomething : unit -> unit [@@qinvokable]
  val someProperty : unit -> int
  val setSomeProperty : int -> unit
end = struct
  type state = { mutable count : int }

  let state = { count = 0 }
  let someProperty () = state.count
  let () = ()

  let setSomeProperty newval =
    if state.count <> newval
    then (
      state.count <- newval;
      !self_container
      |> Result.map (fun this -> somePropertyChanged this newval)
      |> fun x -> assert (Result.is_ok x))
  ;;

  let doSomething () = ()
end [@qml
      singleton
        ~name:"Singleton1"
        ((someProperty : int)
           READ
           someProperty
           WRITE
           setSomeProperty
           NOTIFY
           somePropertyChanged)]

let () = ()

(* include (
  struct
    let doSomething () = ()
    let someProperty () = 5
    let setSomeProperty newval = ()
  end [@qml
        singleton
          ~name:"MyApi"
          ((someProperty : int)
             READ
             somePropery
             WRITE
             setSomeProperty
             NOTIFY
             somePropertyChanged)] :
    sig
      (* [%property int someProperty READ somePropery WRITE setSomeProperty NOTIFY somePropertyChanged] *)
      (* [%qml_named_element MyApi] *)
      val doSomething : unit -> unit [@@qinvokable]
    end)
 *)
