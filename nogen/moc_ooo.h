/****************************************************************************
** Meta object code from reading C++ file 'ooo.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.2.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "ooo.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ooo.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.2.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
struct qt_meta_stringdata_MyDynamicQObject_t {
    QByteArrayData data[6];
    char stringdata[45];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    offsetof(qt_meta_stringdata_MyDynamicQObject_t, stringdata) + ofs \
        - idx * sizeof(QByteArrayData) \
    )
static const qt_meta_stringdata_MyDynamicQObject_t qt_meta_stringdata_MyDynamicQObject = {
    {
QT_MOC_LITERAL(0, 0, 16),
QT_MOC_LITERAL(1, 17, 7),
QT_MOC_LITERAL(2, 25, 0),
QT_MOC_LITERAL(3, 26, 5),
QT_MOC_LITERAL(4, 32, 5),
QT_MOC_LITERAL(5, 38, 5)
    },
    "MyDynamicQObject\0signal1\0\0slot1\0slot2\0"
    "meth1\0"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_MyDynamicQObject[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: name, argc, parameters, tag, flags
       1,    0,   34,    2, 0x06,

 // slots: name, argc, parameters, tag, flags
       3,    0,   35,    2, 0x0a,
       4,    1,   36,    2, 0x0a,

 // methods: name, argc, parameters, tag, flags
       5,    0,   39,    2, 0x02,

 // signals: parameters
    QMetaType::Void,

 // slots: parameters
    QMetaType::Int,
    QMetaType::Void, QMetaType::Int,    2,

 // methods: parameters
    QMetaType::Void,

       0        // eod
};

void MyDynamicQObject::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        MyDynamicQObject *_t = static_cast<MyDynamicQObject *>(_o);
        switch (_id) {
        case 0: _t->signal1(); break;
        case 1: { int _r = _t->slot1();
            if (_a[0]) *reinterpret_cast< int*>(_a[0]) = _r; }  break;
        case 2: _t->slot2((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: _t->meth1(); break;
        default: ;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        void **func = reinterpret_cast<void **>(_a[1]);
        {
            typedef void (MyDynamicQObject::*_t)();
            if (*reinterpret_cast<_t *>(func) == static_cast<_t>(&MyDynamicQObject::signal1)) {
                *result = 0;
            }
        }
    }
}

const QMetaObject MyDynamicQObject::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MyDynamicQObject.data,
      qt_meta_data_MyDynamicQObject,  qt_static_metacall, 0, 0}
};


const QMetaObject *MyDynamicQObject::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *MyDynamicQObject::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MyDynamicQObject.stringdata))
        return static_cast<void*>(const_cast< MyDynamicQObject*>(this));
    return QObject::qt_metacast(_clname);
}

int MyDynamicQObject::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 4)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void MyDynamicQObject::signal1()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
