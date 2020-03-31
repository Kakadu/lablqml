#ifndef MESSAGEBOARD_H
#define MESSAGEBOARD_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>

#include "messageboardattachedtype.h"
#include "message.h"

/*
  class messageBoard cppobj = object
    method messages v = QQmlList.of_list [ ... ]
  end [@@qml element
           ~attached:MessageBoardAttachedType
           ~default_property:messages
           [ property QQmlListProperty<Message> messages READ messages
           ]
      ]

expands into

  class messageBoard cppobj = object
    method handle = cppobj
    method messages v = QQmlList.of_list [ ... ]
  end

  let create_messageBoard_from_cpp obj = new messageBoard obj
  let () =
    Callback.register "create_messageBoard_from_cpp" create_messageBoard_from_cpp
*/

class MessageBoard : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QQmlListProperty<Message> messages READ messages)
    Q_CLASSINFO("DefaultProperty", "messages")
    QML_ATTACHED(MessageBoardAttachedType)
    QML_ELEMENT
public:
    static MessageBoardAttachedType *qmlAttachedProperties(QObject *object)
    {
        return new MessageBoardAttachedType(object);
    }
public:
    QQmlListProperty<Message> messages() {
        return QQmlListProperty<Message>(this, &m_messages);
    }

private:
    QList<Message *> m_messages;
};

#endif // MESSAGEBOARD_H
