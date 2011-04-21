(* $Id: qt.ml,v 1.10 2008/03/10 08:51:14 garrigue Exp $ *)


open StdLabels
open MoreLabels

type 'a obj
(*
module Virtual = struct
  let table : (Obj.t, (Obj.t, Obj.t) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 47
  let lookup (obj : Obj.t) (meth : Obj.t) =
    try Hashtbl.find (Hashtbl.find table obj) meth
    with Not_found -> Obj.repr 0
  and delete obj =
    try Hashtbl.remove table obj
    with Not_found -> ()
  let _ =
    Callback.register "lookup_method" lookup;
    Callback.register "delete_object" delete

  let override (obj : [> `override of 'a] obj) (meths : 'a list) =
    let tbl =
      try Hashtbl.find table (Obj.repr obj)
      with Not_found ->
	let tbl = Hashtbl.create 7 in
	Hashtbl.add table ~key:(Obj.repr obj) ~data:tbl;
	tbl
    in
    List.iter meths ~f:
      begin fun meth ->
	let meth = Obj.repr meth in
	if Obj.is_block meth && Obj.size meth = 2 then
	  Hashtbl.add tbl ~key:(Obj.field meth 0) ~data:(Obj.field meth 1)
	else
	  invalid_arg "Qt.Virtual.override"
      end
end
  *)
module QObject = struct
  external connect :
    'a obj -> signal:string -> dst:'b obj -> slot:string -> bool
    = "ml_QObject_connect"
  external objectName : 'a obj -> string = "ml_QObject_objectName"
  external setObjectName : string -> 'q obj -> unit = "ml_QObject_setObjectName"
  let connect obj ~signal ~dst ~slot =
    connect obj ~dst ~signal:(signal) ~slot:(slot)
end

module QApplication = struct
  type t = [`qapplication]
  external create : string array -> t obj = "ml_QApplication"
  external argv : [> `qapplication] obj -> string array
      = "ml_QApplication_argv"
(*  external setMainWidget : [> `qapplication] obj -> [> `qwidget] obj -> unit
      = "ml_QApplication_setMainWidget" *)
  external exec : [> `qapplication] obj -> int = "ml_QApplication_exec"
end
