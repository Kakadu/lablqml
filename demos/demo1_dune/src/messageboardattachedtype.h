#ifndef MESSAGEBOARDATTACHEDTYPE_H
#define MESSAGEBOARDATTACHEDTYPE_H

#include <QtCore/QObject>
#include <QtCore/QDebug>
#include <QtQuick/QQuickItem>

/*
  class messageBoardAttachedType cppobj = object
    method expired () = ...
    method setExpired v = ...
  end [@@qml anonymous
           [ property bool expired READ expired WRITE setExpired NOTIFY expiredChanged
           ]
      ]

expands into

  class messageBoardAttachedType cppobj = object
    method handle = cppobj
    method expired () = ...
    method setExpired v = ...
  end

  let create_messageBoardAttachedType_from_cpp obj = new messageBoardAttachedType obj
  external emit_expiredChanged : cppobj -> bool -> unit = "..."

  let () =
    Callback.register "create_messageBoardAttachedType_from_cpp" create_messageBoardAttachedType_from_cpp
*/

class MessageBoardAttachedType : public QObject
{
    Q_OBJECT
    Q_PROPERTY(bool expired READ expired WRITE setExpired NOTIFY expiredChanged)
    QML_ANONYMOUS
public:
    explicit MessageBoardAttachedType(QObject *parent = nullptr) {
        Q_UNUSED(parent);
    }

    bool expired() const { return m_expired; }
    void setExpired(bool expired) {
        if (expired != m_expired) {
            m_expired = expired;
            qDebug() << Q_FUNC_INFO;
            emit expiredChanged(m_expired);
        }
    }

signals:
    void published();
    void expiredChanged(bool);

private:
    bool m_expired;
};

#endif // MESSAGEBOARDATTACHEDTYPE_H
