#include "headers.h"
#include <caml/mlvalues.h>


class OCamlBindingObject {
protected:	
	value _camlobj;
public:
	void setCamlObj(value x) { _camlobj = x; }
};

