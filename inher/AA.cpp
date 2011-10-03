#include "A.h"
#include "headers.h"
class AA : public A {
	public:
		value camlobj;



};


/*
extern "C" {
value create_AA(value camlobj) {
  CAMLparam1(camlobj);
  AA* aa = new AA;
  aa -> camlobj = camlobj;
  CAMLreturn((value)aa);
}

}
*/


