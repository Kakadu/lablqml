#ifndef KAMLO_H
#define KAMLO_H

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
}

#define Val_none Val_int(0)
static inline value
Some_val(value v) {
  CAMLparam1(v);
  CAMLlocal1(ans);
  ans = caml_alloc_small(1,0);
  Field(ans,0)=v;
  CAMLreturn(ans);
}
/*
#define Kamlolist_of_QList(conv,lst,ans_name) \
{\
	if (lst.length()==0) \
		ans_name = Val_emptylist;\
	else {\
		auto i = lst.end() --;\
		for (;;) {\
			
	}\
}
*/
#endif
