//enum WindowType (CustomizeWindowHint WindowTitleHint FramelessWindowHint WindowType_Mask SubWindow Desktop SplashScreen ToolTip Tool Popup Drawer Sheet Dialog Window Widget)

#include <Qt/QtOpenGL>
#pragma GCC diagnostic ignored "-Wswitch"
#include "headers.h"
extern "C" {
Qt::WindowFlags enum_of_caml_Qt_WindowFlags(value v) {
  if (v==caml_hash_variant("CustomizeWindowHint")) return Qt::CustomizeWindowHint;
  if (v==caml_hash_variant("WindowTitleHint")) return Qt::WindowTitleHint;
  if (v==caml_hash_variant("FramelessWindowHint")) return Qt::FramelessWindowHint;
  if (v==caml_hash_variant("WindowType_Mask")) return Qt::WindowType_Mask;
  if (v==caml_hash_variant("SubWindow")) return Qt::SubWindow;
  if (v==caml_hash_variant("Desktop")) return Qt::Desktop;
  if (v==caml_hash_variant("SplashScreen")) return Qt::SplashScreen;
  if (v==caml_hash_variant("ToolTip")) return Qt::ToolTip;
  if (v==caml_hash_variant("Tool")) return Qt::Tool;
  if (v==caml_hash_variant("Popup")) return Qt::Popup;
  if (v==caml_hash_variant("Drawer")) return Qt::Drawer;
  if (v==caml_hash_variant("Sheet")) return Qt::Sheet;
  if (v==caml_hash_variant("Dialog")) return Qt::Dialog;
  if (v==caml_hash_variant("Window")) return Qt::Window;
  if (v==caml_hash_variant("Widget")) return Qt::Widget;
  printf("if u see this line, the thereis a bug in enum generation");
  return Qt::CustomizeWindowHint;
}

value enum_to_caml_Qt_WindowFlags(Qt::WindowFlags e) {
  switch (e) {
    case Qt::CustomizeWindowHint: return hash_variant("CustomizeWindowHint");
    case Qt::WindowTitleHint: return hash_variant("WindowTitleHint");
    case Qt::FramelessWindowHint: return hash_variant("FramelessWindowHint");
    case Qt::WindowType_Mask: return hash_variant("WindowType_Mask");
    case Qt::SubWindow: return hash_variant("SubWindow");
    case Qt::Desktop: return hash_variant("Desktop");
    case Qt::SplashScreen: return hash_variant("SplashScreen");
    case Qt::ToolTip: return hash_variant("ToolTip");
    case Qt::Tool: return hash_variant("Tool");
    case Qt::Popup: return hash_variant("Popup");
    case Qt::Drawer: return hash_variant("Drawer");
    case Qt::Sheet: return hash_variant("Sheet");
    case Qt::Dialog: return hash_variant("Dialog");
    case Qt::Window: return hash_variant("Window");
    case Qt::Widget: return hash_variant("Widget");
  }
  printf("if u see this line, the thereis a bug in enum generation");
  return Qt::CustomizeWindowHint;

}

}
