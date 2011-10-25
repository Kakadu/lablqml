#include <Qt/QtOpenGL>
#include "headers.h"
extern "C" {

value ml_qapp_create (value argv) {
  CAMLparam1(argv);
  CAMLlocal1(_ans);
  int argc = Wosize_val(argv);
  char **copy = new char*[argc];
  for (int i = 0; i < argc; i++) {
    int l = string_length(Field(argv,i));
    copy[i] = strcpy (new char[l+1], String_val(Field(argv,i)));
  }
  _ans = (value)(new QApplication (argc, copy));
  printf("QApplication created\n");
  CAMLreturn(_ans);
}

value ml_qapp_exec (value self) {
  CAMLparam1(self);
  CAMLreturn( Val_int (((QApplication*)self)->exec()) );
}
CAMLprim 
value ml_QObject_connect (value sender, value signal, value receiver, value member) {
  CAMLparam4(sender,signal,receiver,member);
  char *sg = String_val(signal);
  char *sl = String_val(member);
  int len1 = strlen(sg), len2 = strlen(sl);
  char cc1[len1+2];
  char cc2[len2+2];
//  printf("signal = %s, slot = %s\n", sg, sl);
  cc1[len1+1] = cc2[len2+1] = '\0';
  cc1[0] = '2'; cc2[0] = '1';
  int i=1;
  for (   ;i<=len1; ++i) cc1[i] = sg[i-1];
  for (i=1;i<=len2; ++i) cc2[i] = sl[i-1];

//  QString s1 = QString("2%1").arg(QString(sg)),
//	  s2 = QString("1%1").arg(QString(sl));
//  printf("signal = %s, slot = %s\n", s1.toLocal8Bit().data(), s2.toLocal8Bit().data() );
//  const char *loc1 = s1.toLocal8Bit().data();
//  const char *loc2 = s2.toLocal8Bit().data();
  printf ("trying to connect %s -> %s\n", cc1, cc2);
  CAMLreturn( 
    Val_bool
      (QObject::connect(QObject_val(sender),  
                        cc1,
                        QObject_val(receiver),
                        cc2) ) );
}
  //============================ settting and getting caml object from QObject
  #define CAMLOBJ_PROPERTY "_camlobj"
  value camlObj(const QObject* o) {
    QVariant ans = o -> property(CAMLOBJ_PROPERTY);
    if (ans.isValid())
	    return ans.toLongLong();
    else
	    return 0;
  }
  CAMLprim // [`qobject] obj -> 'a option
  value hasCamlObj(value cppobj) {
    CAMLparam1(cppobj);
    QObject *o = (QObject*)cppobj;
    value ans = camlObj(o);
    if (ans != 0)
      CAMLreturn( Some_val((value)ans) );
    else
      CAMLreturn(Val_none);
  }

  CAMLprim
  value setCamlObj(value cppobj, value camlobj) {
    CAMLparam2(cppobj, camlobj);
    QObject *o = (QObject*)cppobj;
    o->setProperty(CAMLOBJ_PROPERTY, (qlonglong)camlobj);
    CAMLreturn(Val_unit);
  }
  
  CAMLprim // [`qobject ] obj -> string option 
  value getClassName(value cppobj) {
    CAMLparam1(cppobj);
    QObject *qobj = (QObject*)cppobj;
    if (qobj == NULL)
      CAMLreturn(Val_none);
    else 
      CAMLreturn(Some_val(caml_copy_string(qobj -> metaObject() -> className() ) ) );
  }

}  // extern "C"

