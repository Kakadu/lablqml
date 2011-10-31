open Stub_helpers


(* ********** class QKeyEvent *********** *)
  (* method int key( ) *)
external qKeyEvent_key': 'a->int
		= "native_pub_QKeyEvent_key"

(* ********** class QObject *********** *)
  (* method QObject* parent( ) *)
external qObject_parent': 'a->[> `qobject] obj option
		= "native_pub_QObject_parent"
  (* method void setParent(QObject*   ) *)
external qObject_setParent': 'a->[> `qobject] obj->unit
		= "native_pub_QObject_setParent_QObject"
  (* method bool signalsBlocked( ) *)
external qObject_signalsBlocked': 'a->bool
		= "native_pub_QObject_signalsBlocked"

(* ********** class QWidget *********** *)
  (* method void show( ) *)
external qWidget_show': 'a->unit
		= "native_pub_QWidget_show"
  (* method void keyPressEvent(QKeyEvent*   ) *)
external qWidget_keyPressEvent': 'a->[> `qobject] obj->unit
		= "native_prot_QWidget_keyPressEvent_QKeyEvent"
  (* method QObject* parent( ) *)
external qWidget_parent': 'a->[> `qobject] obj option
		= "native_pub_QObject_parent"
  (* method void setParent(QObject*   ) *)
external qWidget_setParent': 'a->[> `qobject] obj->unit
		= "native_pub_QObject_setParent_QObject"
  (* method bool signalsBlocked( ) *)
external qWidget_signalsBlocked': 'a->bool
		= "native_pub_QObject_signalsBlocked"
