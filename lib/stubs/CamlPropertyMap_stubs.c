#include "CamlPropertyMap.h"

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>

extern "C"
value_hack caml_create_camlpropertymap(value_hack _cb)
{
  CAMLparam1(_cb);
  CAMLlocal1(_ans);
  caml_enter_blocking_section();

  _ans = caml_alloc_small(1, Abstract_tag);
  CamlPropertyMap *map = new CamlPropertyMap();
  map->saveCallback(&_cb);
  (*((CamlPropertyMap **) &Field(_ans, 0))) = map;

  caml_leave_blocking_section();
  CAMLreturn(_ans);
}
//-----------------------------------------------------------
CamlPropertyMap::CamlPropertyMap()
  : QQmlPropertyMap(this, NULL), _saved_callback(NULL)
{
}

CamlPropertyMap::~CamlPropertyMap()
{
    if (_saved_callback != NULL)
        caml_remove_global_root(_saved_callback);
}

void CamlPropertyMap::saveCallback(value_hack *_cb)
{
      if (_saved_callback != NULL)
          caml_remove_global_root(_saved_callback);

      _saved_callback = _cb;
      if (_saved_callback != NULL)
          caml_register_global_root(_saved_callback);
}
