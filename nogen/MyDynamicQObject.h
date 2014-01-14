#ifndef MY_DYNAMIC_QOBJECT_H
#define MY_DYNAMIC_QOBJECT_H

#include <QtCore/QObject>
#include <QtCore/QHash>
#include <QtCore/QDebug>

#include <kamlo.h>

typedef QString CamlMethName;
typedef QString CamlMethSign;

struct qt_meta_stringdata_Ooo_t {
  QByteArrayData* data;
  char* stringdata;
};

#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    offsetof(qt_meta_stringdata_Ooo_t, stringdata) + ofs \
        - idx * sizeof(QByteArrayData) \
    )

class MyDynamicQObject : public QObject {
public:
  template <typename ThisObject>
  inline void qt_check_for_QOBJECT_macro(const ThisObject &_q_argument) const {
    int i = qYouForgotTheQ_OBJECT_Macro(this, &_q_argument);
    i = i + 1;
  }

  // next three fields contain information which methods are available
  qt_meta_stringdata_Ooo_t* qt_meta_stringdata_Ooo;
  uint* qt_meta_data_Ooo;
  QMetaObject* a_la_staticMetaObject;

  char* className;
  void reevalThreeFields() {
      char* names_str;
      int name_len = strlen(className);
      int n = name_len + 1, l = n;

      foreach(auto item, q_invokables) {
          n+= 1 + item.first.length();
      }
      names_str = new char[n];

      memcpy(names_str, className, name_len);
      names_str[name_len] = '\0';

      qt_meta_stringdata_Ooo = new qt_meta_stringdata_Ooo_t;
      qt_meta_stringdata_Ooo->data = new QByteArrayData[1+q_invokables.length()];
      //QArrayData d = { { { (-1) } } , 1,1,1,1 };
      QArrayData d = QT_MOC_LITERAL(0, 0, name_len);
      d.ref.atomic.store(-1);
      memcpy(qt_meta_stringdata_Ooo->data, &d, sizeof(QArrayData));

      for (uint i=0; i<q_invokables.length(); ++i) {
          auto item = q_invokables[i];
          int curlen = item.first.length();
          const char* s2 = item.first.toLatin1().data();
          memcpy(names_str + l, s2, curlen+1); // +1 because last '\0'
          names_str[l+curlen] = '\0';
          QByteArrayData d = QT_MOC_LITERAL(i+1, l, curlen); /*
          {
              { { (-1) } }, 16, 0, 0,
              __builtin_offsetof (qt_meta_stringdata_MyDynamicQObject_t, stringdata) + 0 - 0 * sizeof(QByteArrayData)
          };*/
          //qt_meta_stringdata_Ooo->data[i+1] = d; //
          memcpy(qt_meta_stringdata_Ooo->data + (i+1)*sizeof(QArrayData), &d, sizeof(QArrayData));
          l+= curlen+1;
      }
      qt_meta_stringdata_Ooo->stringdata = names_str;
      qDebug() << "PIZDA";
      qDebug() << names_str;
  }

  virtual const QMetaObject* metaObject() const;
  virtual void *qt_metacast(const char *) {
    Q_ASSERT(false);
  }
  static inline QString tr(const char *s, const char *c = 0, int n = -1) {
    Q_ASSERT(false);
    return staticMetaObject.tr(s, c, n);
  }
  static inline QString trUtf8(const char *s, const char *c = 0, int n = -1) {
    return staticMetaObject.tr(s, c, n);
  }

public:

  virtual int qt_metacall(QMetaObject::Call c, int id, void **arguments);

  void storeCAMLobj(value x) {
    if (_camlobjHolder != 0) {
       //maybe unregister global root?
    }
    _camlobjHolder = x;
    register_global_root(&_camlobjHolder);
  }

  void addSlot(const QString&, const QString& desc);

  MyDynamicQObject() {

      qt_meta_data_Ooo = new uint[21] {
              // content:
              7,       // revision
              0,       // classname
              0,    0, // classinfo
              1,   14, // methods
              0,    0, // properties
              0,    0, // enums/sets
              0,    0, // constructors
              0,       // flags
              0,       // signalCount
              // methods: name, argc, parameters, tag, flags
              1,    0,   19,    2, 0x02,
              // methods: parameters
              QMetaType::Void,
              0        // eod
      };
      char *str = new char[11];
      strcpy(str, "Ooo\0meth1\0\0");
      className = new char[17];
      strcpy(className, "MyDynamicQObject");

      reevalThreeFields();
      this->a_la_staticMetaObject = new QMetaObject();
      *a_la_staticMetaObject = makeMetaObject();
  }

private:
  QList<QPair<CamlMethName,CamlMethSign> > q_invokables;
  __attribute__((visibility("hidden")))
  static void qt_static_metacall(QObject *, QMetaObject::Call, int, void **);

  void wrap_qt_static_metacall(QObject *, QMetaObject::Call, int, void **) {
    Q_ASSERT_X(false,"wrap_qt_static_metacall","");
  }

  QMetaObject makeMetaObject() {
    QMetaObject o = {
      { &QObject::staticMetaObject, qt_meta_stringdata_Ooo->data,
        qt_meta_data_Ooo, qt_static_metacall, 0, 0 }
    };
    return o;
  }
  value _camlobjHolder;
};
#endif
