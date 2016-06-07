open Qtstubs
open Classes
open Creators
open Stubs
open Stub_helpers
(*open UserSlots_stubs *)
open Printf 

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |] 
 (* HACK to avoid bug with optimization levels *)

let dialog = create_QDialog_0 None `Dialog
let menuBar = create_QMenuBar_0 None
let fileMenu = create_QMenu_1 "&File" (Some (dialog:> qWidget) )
let exitAction = match fileMenu#addAction_ "E&xit" with
  | Some x -> x
  | None -> assert false
let _ = connect exitAction exitAction#signal_triggered dialog dialog#slot_accept
let _ = menuBar#addMenu fileMenu

(* create horizaontal group box *)
let horGroupBox = create_QGroupBox_1 "Horizontal Layout" None 
let layout = create_QHBoxLayout_0 ()
let () = for i=0 to 3 do
                let b = create_QPushButton_1 (sprintf "Button %d" i) None in
                layout#addWidget ((b) :> qWidget)
         done;
         horGroupBox#setLayout (layout :> qLayout)

(* create grid groupBox *)         
let gridGroupBox = create_QGroupBox_1 "Grid layout" None
let () = 
        let layout = create_QGridLayout_1 () in
        for i=0 to 2 do 
          let label = create_QLabel_1 (sprintf "Line %d:" i) None `Widget in
          let lineEdit = create_QLineEdit_1 
            "" None in
          layout#addWidget_ (label   :> qWidget) (i+1) 0 `AlignLeft;
          layout#addWidget_ (lineEdit:> qWidget) (i+1) 1 `AlignLeft
        done;
        let smallEditor = create_QTextEdit_1 "This widget takes up about two thirds of the grid layout" None in
        layout#addWidget__ (smallEditor :> qWidget) 0 2 4 1 `AlignRight;
        layout#setColumnStretch 1 10;
        layout#setColumnStretch 2 20;
        gridGroupBox#setLayout (layout :> qLayout)

(* create Form group box *)
let formGroupBox = create_QGroupBox_1 "Form Layout" None 
let () =
        let layout = create_QFormLayout_0 None in
        layout#addRow_____ 
          ((create_QLabel_1 "Line 1:" None `Widget) :> qWidget)
          ((create_QLineEdit_0 None) :> qWidget);

        layout#addRow_____ 
          ((create_QLabel_1 "Line 2, long text:" None `Widget) :> qWidget)
          ((create_QComboBox_0 None) :> qWidget);

        layout#addRow_____ 
          ((create_QLabel_1 "Line 3:" None `Widget) :> qWidget)
          ((create_QSpinBox_0 None) :> qWidget);
        formGroupBox#setLayout (layout :> qLayout)

(* last part *)
let mainLayout = create_QVBoxLayout_0 ()
let bigEditor = create_QTextEdit_1 
    "This widget takes up all the remaining space in the top-level layout." None
let buttonBox = create_QDialogButtonBox_0 None 
let () =
    let _ = buttonBox#addButton `Ok in
    let _ = buttonBox#addButton `Cancel in
    let () = 
      connect buttonBox buttonBox#signal_accepted dialog  dialog#slot_accept in
    let () = 
      connect buttonBox buttonBox#signal_rejected dialog  dialog#slot_reject in
    ()
    
let () = 
        mainLayout#setMenuBar (menuBar :> qWidget);
        mainLayout#addWidget  (horGroupBox :> qWidget);
        mainLayout#addWidget  (gridGroupBox :> qWidget);
        mainLayout#addWidget  (formGroupBox :> qWidget);
        mainLayout#addWidget  (bigEditor    :> qWidget);
        mainLayout#addWidget  (buttonBox    :> qWidget);
        dialog#setLayout      (mainLayout :> qLayout);
        dialog#slot_setWindowTitle#call "Basic Layouts"

(* WARNING: in Qt you should add something on a widget and that show this widget
 * in reverse order you'll not see this `something` *)

let () = dialog#slot_show#call
let _ = QApplication.exec app

