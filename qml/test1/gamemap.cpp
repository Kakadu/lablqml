#include "gamemap.h"

GameMap::GameMap(QObject *parent) :
    QObject(parent)
{
    _title = "Some title";
}

int GameMap::sizex() {
    CAMLparam0();
    CAMLlocal1(ans);
    static value* closure_f = NULL;
    if (closure_f == NULL)
        closure_f = caml_named_value("getwidth");
    Q_ASSERT(closure_f!=NULL);

    fflush(stderr);
    ans = caml_callback(*closure_f, Val_unit);
    int _ans = Int_val(ans);
    return _ans;
}
