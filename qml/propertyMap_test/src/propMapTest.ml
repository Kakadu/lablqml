open QmlContext

let () = Printexc.record_backtrace true

module PropMap: sig
  type t
  val handler: t -> QmlContext.cppobj

  val create: unit -> t
  val insert: t -> name:string -> QVariant.t -> unit
end = struct
  type t = QmlContext.cppobj

  let handler: t -> QmlContext.cppobj = fun x -> x

  external create_stub: unit -> QmlContext.cppobj = "caml_create_QQmlPropertyMap"
  external insert_stub: t -> string -> QVariant.t -> unit = "caml_QQmlPropertyMap_insert"

  let create () = create_stub ()
  let insert map ~name variant = insert_stub map name variant

end

let main () =
  let map = PropMap.create () in
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
