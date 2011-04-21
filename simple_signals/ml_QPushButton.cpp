#include <qpushbutton.h>
#include <stdio.h>
#include "headers.h"
#include <qdebug.h>
extern "C" {
  value ml_qWidget_show(value self) {    
    CAMLparam1(self);
    QWidget* w = (QWidget*) self;
    //QString s = w->objectName();
    w -> show();
    CAMLreturn(Val_unit);
  }
  value ml_createButton(value s) {
    CAMLparam1 (s);
    CAMLlocal1(ans);
    ans = (value) (new QPushButton(QString(String_val(s))));
    CAMLreturn(ans);
   }
  value ml_setButtonChecked(value sender, value b) {
    CAMLparam2(sender,b);
    bool bb = Val_bool(b);
    qDebug() << "setChecked: " << bb;
    ((QPushButton*)(sender)) -> setChecked(bb);
    CAMLreturn(Val_unit);
  }
  value ml_setButtonCheckable(value sender, value b) {
    CAMLparam2(sender,b);
    bool bb = Val_bool(b);
    qDebug() << "setCheckable: " << bb;
    ((QPushButton*)sender) -> setCheckable(bb);
    CAMLreturn(Val_unit);
  }


//QPushButton( const (QString & )text,QWidget * parent = 0);
value ml_QPushButton_2(value text,value parent) {
	CAMLparam2 (text, parent);
	CAMLlocal1(qt_obj_instance);
	qt_obj_instance = (value)(new QPushButton(QString(String_val(text)),( (parent==Val_none) ? 0 : ((QWidget*) (Some_val(parent)) ) )));
	CAMLreturn(qt_obj_instance);
}
//bool autoDefault() const ;
value ml_QPushButton_autoDefault_0(value self) {
	CAMLparam1 (self);
	CAMLlocal1(calling_result);
	calling_result = Val_bool(((QPushButton*)self) ->  autoDefault());
	CAMLreturn(calling_result);
}
//bool isDefault() const ;
value ml_QPushButton_isDefault_0(value self) {
	CAMLparam1 (self);
	CAMLlocal1(calling_result);
	calling_result = Val_bool(((QPushButton*)self) ->  isDefault());
	CAMLreturn(calling_result);
}
//bool isFlat() const ;
value ml_QPushButton_isFlat_0(value self) {
	CAMLparam1 (self);
	CAMLlocal1(calling_result);
	calling_result = Val_bool(((QPushButton*)self) ->  isFlat());
	CAMLreturn(calling_result);
}
//void setAutoDefault(bool );
value ml_QPushButton_setAutoDefault_1(value arg0,value self) {
	CAMLparam2 (arg0, self);
	((QPushButton*)self) ->  setAutoDefault(Bool_val(arg0));
	CAMLreturn(Val_unit);
}
//void setDefault(bool );
value ml_QPushButton_setDefault_1(value arg0,value self) {
	CAMLparam2 (arg0, self);
	((QPushButton*)self) ->  setDefault(Bool_val(arg0));
	CAMLreturn(Val_unit);
}
//void setFlat(bool );
value ml_QPushButton_setFlat_1(value arg0,value self) {
	CAMLparam2 (arg0, self);
	((QPushButton*)self) ->  setFlat(Bool_val(arg0));
	CAMLreturn(Val_unit);
}
//void setFocus();
value ml_QPushButton_setFocus_0(value self) {
	CAMLparam1 (self);
	((QPushButton*)self) ->  setFocus();
	CAMLreturn(Val_unit);
}
} // extern "C"
