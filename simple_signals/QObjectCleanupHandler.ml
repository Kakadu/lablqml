open Qt
type t = [`qwidget]
  (* QObjectCleanupHandler(); *)
  external create0:  unit  -> [> `qwidget] obj = "ml_QObjectCleanupHandler_0"
  (* QObject * add(QObject * object); *)
  external add: [> `qwidget] obj  -> [> `qwidget] obj -> [> `qwidget] obj = "ml_QObjectCleanupHandler_add_1"
  (* void clear(); *)
  external clear: [> `qwidget] obj -> unit = "ml_QObjectCleanupHandler_clear_0"
  (* bool isEmpty() const ; *)
  external isEmpty: [> `qwidget] obj -> bool = "ml_QObjectCleanupHandler_isEmpty_0"
  (* void remove(QObject * object); *)
  external remove: [> `qwidget] obj  -> [> `qwidget] obj -> unit = "ml_QObjectCleanupHandler_remove_1"
