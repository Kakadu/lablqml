#include <qobjectcleanuphandler.h>
#include <stdio.h>
#include "headers.h"
extern "C" {
//QObjectCleanupHandler();
value ml_QObjectCleanupHandler_0() {
	CAMLparam0 ();
	CAMLlocal1(qt_obj_instance);
	qt_obj_instance = (value)(new QObjectCleanupHandler());
	CAMLreturn(qt_obj_instance);
}
//QObject * add(QObject * object);
value ml_QObjectCleanupHandler_add_1(value object,value self) {
	CAMLparam2 (object, self);
	CAMLlocal1(calling_result);
	calling_result = (value)(((QObjectCleanupHandler*)self) ->  add( ((QObject*)object) ));
	CAMLreturn(calling_result);
}
//void clear();
value ml_QObjectCleanupHandler_clear_0(value self) {
	CAMLparam1 (self);
	((QObjectCleanupHandler*)self) ->  clear();
	CAMLreturn(Val_unit);
}
//bool isEmpty() const ;
value ml_QObjectCleanupHandler_isEmpty_0(value self) {
	CAMLparam1 (self);
	CAMLlocal1(calling_result);
	calling_result = Val_bool (((QObjectCleanupHandler*)self) ->  isEmpty());
	CAMLreturn(calling_result);
}
//void remove(QObject * object);
value ml_QObjectCleanupHandler_remove_1(value object,value self) {
	CAMLparam2 (object, self);
	((QObjectCleanupHandler*)self) ->  remove( ((QObject*)object) );
	CAMLreturn(Val_unit);
}
} // extern "C"
