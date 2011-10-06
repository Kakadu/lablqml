#include "A.h"
#include "headers.h"
#include <caml/mlvalues.h>
#include "WrapperClass.h"
#include <stdio.h>

class AA : public A, protected OCamlBindingObject {
public:
  void foo() {
    CAMLlocal1(meth);
    printf("inside AA::foo\n");
    meth = caml_get_public_method( _camlobj, caml_hash_variant("foo"));
    if (meth==0)
      printf ("total fail\n");
    caml_callback(meth, _camlobj);
    printf ("exit from AA::foo\n");
  }
  virtual void boo() {
    	CAMLlocal1(meth);
	printf ("inside AA::boo\n");
	meth = caml_get_public_method( _camlobj, caml_hash_variant("boo"));
	caml_callback(meth, _camlobj);
	printf ("exit from AA::boo\n");
  }


};

extern "C" {
value create_AA(value unit_x) {
  CAMLparam1(unit_x);
  AA* aa = new AA;
  CAMLreturn((value)aa);
}
value set_caml_obj(value cpp_obj, value caml_obj) {
  CAMLparam2(cpp_obj, caml_obj);
  OCamlBindingObject *o = (OCamlBindingObject*) cpp_obj;
  o -> setCamlObj(caml_obj);
  CAMLreturn(Val_unit);
}

}



