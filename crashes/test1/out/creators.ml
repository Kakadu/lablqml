open Stub_helpers
open Classes
open Stubs
(* class QKeyEvent has pure virtual members - no constructors *)

(* constructor QObject* QObject(QObject* parent  = 0 ) *)
external create_QObject_0' : [> `qobject] obj option -> [>`qobject] obj = "native_pub_createeee_QObject_QObject"
external create_QObject_twin_0' : [> `qobject] obj option -> [>`qobject] obj = "native_pub_createeee_QObject_twin_QObject"
let create_QObject_0 (x0: qObject option) = create_QObject_0' (wrap_handler "native_pub_createeee_QObject_QObject" "x0" x0) 
    |> new qObject
let create_QObject_twin_0 (x0: qObject option) = create_QObject_twin_0' (wrap_handler "native_pub_createeee_QObject_QObject" "x0" x0) 
    |> new qObject

(* constructor QWidget* QWidget(QWidget* parent  = 0, Qt::WindowFlags f  = 0 ) *)
external create_QWidget_0' : [> `qobject] obj option->[`CustomizeWindowHint | `WindowTitleHint | `FramelessWindowHint | `WindowType_Mask | `SubWindow | `Desktop | `SplashScreen | `ToolTip | `Tool | `Popup | `Drawer | `Sheet | `Dialog | `Window | `Widget] -> [>`qobject] obj = "native_pub_createeee_QWidget_QWidget_Qt_WindowFlags"
external create_QWidget_twin_0' : [> `qobject] obj option->[`CustomizeWindowHint | `WindowTitleHint | `FramelessWindowHint | `WindowType_Mask | `SubWindow | `Desktop | `SplashScreen | `ToolTip | `Tool | `Popup | `Drawer | `Sheet | `Dialog | `Window | `Widget] -> [>`qobject] obj = "native_pub_createeee_QWidget_twin_QWidget_Qt_WindowFlags"
let create_QWidget_0 (x0: qWidget option) (x1: [`CustomizeWindowHint | `WindowTitleHint | `FramelessWindowHint | `WindowType_Mask | `SubWindow | `Desktop | `SplashScreen | `ToolTip | `Tool | `Popup | `Drawer | `Sheet | `Dialog | `Window | `Widget]) = 
    create_QWidget_0' (wrap_handler "native_pub_createeee_QWidget_QWidget_Qt_WindowFlags" "x0" x0) x1 
    |> new qWidget
let create_QWidget_twin_0 (x0: qWidget option) (x1: [`CustomizeWindowHint | `WindowTitleHint | `FramelessWindowHint | `WindowType_Mask | `SubWindow | `Desktop | `SplashScreen | `ToolTip | `Tool | `Popup | `Drawer | `Sheet | `Dialog | `Window | `Widget]) = 
    create_QWidget_twin_0' (wrap_handler "native_pub_createeee_QWidget_QWidget_Qt_WindowFlags" "x0" x0) x1 
    |> new qWidget
