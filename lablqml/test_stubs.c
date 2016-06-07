#include "stubs.h"
#include <QtCore/QDebug>

extern "C" value Val_QVariant(value _dest, const QVariant& var);

extern "C" value caml_QObject_property(value _qobj, value _name) {
    CAMLparam2(_qobj, _name);
    CAMLlocal1(_ans);

    //qDebug() << Q_FUNC_INFO;

    QObject *obj = (QObject*) (Field(_qobj,0));
    Q_ASSERT_X(obj != NULL, __func__, "Can't extract QObject");

    const QVariant &ans = obj->property(String_val(_name));

    _ans = Val_QVariant(_ans, ans);
    CAMLreturn(_ans);
}


extern "C" value caml_QQuickWindow_as_qobject(value _qwin) {
    CAMLparam1(_qwin);
    CAMLlocal1(_ans);

    //qDebug() << Q_FUNC_INFO;

    QQuickWindow *qwin = (QQuickWindow*) (Field(_qwin,0));
    Q_ASSERT_X(qwin != NULL, __func__, "Can't extract QQuickWindow");

    _ans = caml_alloc_small(1, Abstract_tag);
    (*((QObject **) &Field(_ans, 0))) = dynamic_cast<QObject *>(qwin);
    //qDebug() << __FILE__ << __LINE__;

    CAMLreturn(_ans);
}