open QmlContext

let () = Printexc.record_backtrace true

module PropMap: sig
  type t
  val handler: t -> QmlContext.cppobj

  val create: ?callback:(string -> QVariant.t -> unit) -> unit -> t
  val insert: t -> name:string -> QVariant.t -> unit
end = struct
  type t = QmlContext.cppobj

  let handler: t -> QmlContext.cppobj = fun x -> x

  external create_stub: (string -> QVariant.t -> unit) -> unit -> QmlContext.cppobj = "caml_create_QQmlPropertyMap"
  external insert_stub: t -> string -> QVariant.t -> unit = "caml_QQmlPropertyMap_insert"

  let create ?(callback=fun _ _ -> ()) () =
    let (_:string -> QVariant.t -> unit) = callback in
    create_stub callback ()
  let insert map ~name variant = insert_stub map name variant

end

let main () =
  let callback name _ =
    Printf.printf "property '%s' has been changed\n%!" name;
    ()
  in
  let map = PropMap.create ~callback () in
  PropMap.insert map ~name:"title" (QVariant.of_string "hello");
  PropMap.insert map ~name:"count" (QVariant.of_int 0);
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"propMap1" (PropMap.handler map);

  let (_: Thread.t) =
    let counter = ref 0 in
    let rec f () =
      let _ = Thread.delay 1.0 in
      incr counter;
      PropMap.insert map ~name:"count" (QVariant.of_int !counter);
      f ()
    in
    Thread.create f ()
  in
  ()

let _ =
  run_with_QQmlApplicationEngine Sys.argv main "qrc:///ui/Root.qml"
