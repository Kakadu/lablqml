open Qtstubs
open Classes
open Creators
open Stubs
open Stub_helpers
open UserSlots_stubs
open Printf 

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |] 
 (* HACK to avoid bug with optimization levels *)
let rootWidget = create_QWidget_0 None `Widget;;
let rootLayout = create_QVBoxLayout_1 rootWidget ;;

let tabWidget = create_QTabWidget_0 None;;
let create_conf_tab name =
        let tab = create_QWidget_0 None `Widget in
        let lay = create_QHBoxLayout_1 tab in
        let users = create_QListWidget_0 None in
        let text = create_QTextEdit_1 "Some text" None in
        List.iter users#addItem_ ["ermine"; "Kakadu"];
        lay#addWidget (text :> qWidget);
        lay#addWidget (users :> qWidget);
        ignore(tabWidget#addTab tab name);
        tab
;;

let tab1 = create_conf_tab "tab1" ;;
let tab2 = create_conf_tab "tab2" ;;
rootLayout#addWidget (tabWidget :> qWidget);;

let messageEdit = create_QLineEdit_1 "text" None;;
rootLayout#addWidget (messageEdit :> qWidget);;
(* *************** Menu ********************** *)

let menuBar = create_QMenuBar_0 None
let exitAction = match menuBar#addAction_ "E&xit" with
  | Some x -> x
  | None -> assert false
(*let _ = connect exitAction exitAction#signal_triggered dialog
 * dialog#slot_accept *)
let joinAction = match menuBar#addAction_ "&Join" with
  | Some x -> x
  | None -> assert false;;

rootLayout#setMenuBar (menuBar :> qWidget);;

(* WARNING: in Qt you should add something on a widget and that show this widget
 * in reverse order you'll not see this `something` *)

let () = rootWidget#slot_show#call
let _ = QApplication.exec app

