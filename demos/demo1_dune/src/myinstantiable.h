#ifndef MYINSTANTIABLE_H
#define MYINSTANTIABLE_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>
#include <QtCore/QDateTime>

/*
  class myInstantiable cppobj = object
    method author () = ...
    method setAuthor v = ...
  end [@@qml element
           [ property string author READ author WRITE setAuthor NOTIFY authorChanged
           ]
      ]

expands into

  class myInstantiable cppobj = object
    method handle = cppobj
    method author () = ...
    method setAuthor v = ...
  end [@@qml element]

  let create_myInstantiable_from_cpp obj = new myInstantiable obj
  external emit_authorChanged : cppobj -> string -> unit = "..."

  let () =
    Callback.register "create_myInstantiable_from_cpp" create_myInstantiable_from_cpp
*/

class MyInstantiable : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString author READ author WRITE setAuthor NOTIFY authorChanged)
    Q_PROPERTY(QDateTime creationDate READ creationDate WRITE setCreationDate NOTIFY creationDateChanged)
    QML_ELEMENT

public:
    explicit MyInstantiable(QObject *parent = nullptr) : QObject(parent) {
        Q_UNUSED(parent);
    }


    QString author() { return m_author; }
    QDateTime creationDate() { return m_date; }
    void setAuthor(const QString& s) {
        if (s != m_author) {
            m_author = s;
            emit authorChanged(s);
        }
    }
    void setCreationDate(const QDateTime& s) {
        if (s != m_date) {
            m_date = s;
            emit creationDateChanged(s);
        }
    }

signals:
    void authorChanged(const QString&);
    void creationDateChanged(const QDateTime&);

private:
    QString m_author;
    QDateTime m_date;
};

#endif // MYINSTANTIABLE_H
