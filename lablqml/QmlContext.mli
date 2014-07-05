type t
val get_view_exn : name:string -> t
val get_view : name:string -> t option

type cppobj
val set_context_property: ctx:t -> name:string -> cppobj -> unit

module QVariant : sig
  type t = [ `empty | `string of string | `qobject of cppobj | `int of int ]
  val empty: t
  val of_string: string -> t
  val of_object: cppobj -> t
  val of_int   : int  -> t
end

module QModelIndex : sig
  type t
  val empty: t
  val row: t -> int
  val column: t -> int
  val make: row:int -> column:int -> t
  val to_string: t -> string
end

module QGuiApplication : sig
  type t
  val exec : t -> unit
end
module QQmlEngine : sig type t end

module QQuickWindow : sig
  type t
  val showMaximized : t -> unit
end

(** Creates QGuiApplication. No platform-dependent styling applied. *)
val create_qapplication : string array -> QGuiApplication.t * QQmlEngine.t

(** Creates QQuickWindow using file and QQmlEngine *)
val loadQml : string -> QQmlEngine.t -> QQuickWindow.t option

(** Initializates and open QQuickWindow. Uses platform dependent styling *)
val run_with_QQmlApplicationEngine : string array -> (unit -> unit) -> string -> unit

type qvariantable
type non_qvariantable
class virtual ['valtyp] prop :
  string ->
  object
    method virtual get : 'valtyp
    method name : string
    method virtual set : 'valtyp -> unit
  end
class virtual ['valtyp] qvariant_prop :
  string ->
  object
    method virtual get : 'valtyp
    method name : string
    method virtual set : 'valtyp -> unit
    method virtual wrap_in_qvariant : 'valtyp -> QVariant.t
  end
