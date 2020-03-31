#ifndef MYSINGLETON_H
#define MYSINGLETON_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>

/*
    module MySingleton : sig
      (* [%property int someProperty READ somePropery WRITE setSomeProperty NOTIFY somePropertyChanged] *)
      [%qml_named_element MyApi]
      val doSomething: unit -> unit [@@qinvokable]
    end = struct
      let doSomething = ...
      let someProperty () = 5
      let setSomeProperty newval = ...

    end[@@qml singleton
           ~name:MyApi
           [ property int someProperty READ somePropery WRITE setSomeProperty NOTIFY somePropertyChanged
           ]
       ]

expands into

    module MySingleton : sig
        val someProperty: unit -> unit
        val setSomeProperty: int -> unit
        val doSomething: unit -> unit
    end = struct
      let doSomething = ...
      let someProperty () = ..
      let setSomeProperty newval = ...

      extenal emit_somePropertyChanged: int -> unit = "...."

      let () =
        Callback.register  "doSomething" doSomething;
        Callback.register  "someProperty" someProperty;
        Callback.register  "setSomeProperty" setSomeProperty
    end
*/
class MySingleton : public QObject
{
    Q_OBJECT
    QML_NAMED_ELEMENT(MyApi)
    QML_SINGLETON
    Q_PROPERTY(int someProperty READ someProperty WRITE setSomeProperty NOTIFY somePropertyChanged)
public:
    explicit MySingleton(QObject *parent = nullptr) : QObject(parent)
    {
        Q_UNUSED(parent);
        qDebug() << Q_FUNC_INFO;
    }

    Q_INVOKABLE int doSomething()
    {
        setSomeProperty(5);
        return m_someProperty;
    }

    int someProperty() const { return m_someProperty; }
    void setSomeProperty(int val) {
        if (m_someProperty != val) {
            m_someProperty = val;
            emit somePropertyChanged(val);
        }
    }
signals:
    void somePropertyChanged(int newValue);
private:
    int m_someProperty = 0;
};

#endif // MYSINGLETON_H
