/* $Id: ml_qobject.cpp,v 1.2 1999/01/29 02:04:49 garrigue Exp $ */

#include <qobject.h>

extern "C" {

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <stdio.h>
#include "qwrappers.h"
#include <QString>
#define CONCAT(x,y) x##y
CAMLprim value ml_QObject_connect (value sender, value signal, value receiver,
			  value member)
{
  char *sg = String_val(signal);
  char *sl = String_val(member);
  const char *loc1 = (QString("2")+QString(sg)).toLocal8Bit().data();
  const char *loc2 = (QString("1")+QString(sl)).toLocal8Bit().data();
  return Val_bool
      (QObject::connect(QObject_val(sender),  
			loc1,
			QObject_val(receiver),
			loc2) );
}
  value ml_QObject_objectName(value sender) {
    CAMLparam1(sender);
    CAMLlocal1(s);
    
	QString qs = (((QObject*) sender ) -> objectName () );
	const char *c = qs.toStdString().c_str();
	s = caml_copy_string (c);
	CAMLreturn (s);

}
  value ml_QObject_setObjectName (value str, value sender) {
	CAMLparam2(str,sender);
	char *s = String_val(str);
	printf("setting object name = %s\n",s);
	QString *qstr = new QString(s);
	printf("do it!\n");
	((QObject*)sender) -> setObjectName (*qstr);
	CAMLreturn(Val_unit);
  }
} /* extern "C" */
