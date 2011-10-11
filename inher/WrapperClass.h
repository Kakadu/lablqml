#include "headers.h"
#include <caml/mlvalues.h>


class OCamlBindingObject {
protected:	
	value _camlobj;
public:
	void setCamlObj(value x) { 
	  if (_camlobj != 0)
   	    caml_remove_global_root(&_camlobj);
	  _camlobj = x; 
	  if (_camlobj !=0)
            caml_register_global_root(&_camlobj);
	}
};

