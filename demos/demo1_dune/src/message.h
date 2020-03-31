#ifndef MESSAGE_H
#define MESSAGE_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>
#include <QtCore/QDateTime>

/*
  class myMessage cppobj = object
    method creationDate () = ...
    method setCreationDate v = ...
  end [@@qml element
           [ property QDateTime creationDate READ creationDate WRITE setCreationDate NOTIFY creationDateChanged
           ]
      ]

expands into

  class myMessage cppobj = object
    method handle = cppobj
    method creationDate () = ...
    method setCreationDate v = ...
  end [@@qml element]

  let create_myMessage_from_cpp obj = new myMessage obj
  external emit_creationDateChanged : cppobj -> string -> unit = "..."

  let () =
    Callback.register "create_myMessage_from_cpp" create_myMessage_from_cpp
*/

class Message : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString author READ author WRITE setAuthor NOTIFY authorChanged)
    Q_PROPERTY(QDateTime creationDate READ creationDate WRITE setCreationDate NOTIFY creationDateChanged)
    QML_ELEMENT
public:
    explicit Message(QObject *parent = nullptr) : QObject(parent)
    {
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

#endif // MESSAGE_H
