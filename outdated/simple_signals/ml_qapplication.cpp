/* $Id: ml_qapplication.cpp,v 1.2 1999/01/27 05:37:07 garrigue Exp $ */

#include <qapplication.h>

extern "C" {

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include "qwrappers.h"

#define QApplication_val(val) ((QApplication*)QObject_val(val))

value ml_QApplication (value argv) {
    int argc = Wosize_val(argv);
    char **copy = new char*[argc];
    for (int i = 0; i < argc; i++) {
      int l = string_length(Field(argv,i));
      copy[i] = strcpy (new char[l+1], String_val(Field(argv,i)));
    }
    return Val_QObject(new QApplication (argc, copy));
}

value ml_QApplication_argv (value self)
{
    QApplication *qapp = QApplication_val(self);
    int argc = qapp->argc();
    char **argv = qapp->argv();
    value ret = (argc ? alloc_shr (argc, 0) : Atom(0)), s;
    Begin_root (ret);
    for (int i = 0; i < argc; i++) {
	s = copy_string (argv[i]);
	initialize(&Field(ret,i), s);
    }
    End_roots ();
    return ret;
}
/*    
value ml_QApplication_setMainWidget (value self, value val)
{
    QApplication_val(self)->setMainWidget(QWidget_val(val));
    return Val_unit;
}
*/
value ml_QApplication_exec (value self)
{
    return Val_int (QApplication_val(self)->exec());
}

} /* extern "C" */
