module MySingleton : sig
  val action2 : unit -> unit [@@qinvokable]

  val action1 : unit -> unit [@@qinvokable]

  val someProperty : unit -> int

  val setSomeProperty : int -> unit
end = struct
  let action1 () = print_endline "action1 called"

  let action2 () = print_endline "action2 called"

  type state = { mutable count : int }

  let state = { count = 0 }

  let someProperty () = state.count

  let setSomeProperty newval =
    if state.count <> newval then (
      state.count <- newval;
      !self_container
      |> Result.map (fun this -> somePropertyChanged this newval)
      |> fun x -> assert (Result.is_ok x))
end
[@qml
  singleton ~name:"Singleton1"
    ((someProperty : int)
       READ someProperty WRITE setSomeProperty NOTIFY somePropertyChanged)]

(* slider uses old-style generation of components *)
class virtual myslider =
  object (self)
    method virtual descr : string [@@qtprop]
  end [@@qtclass]
