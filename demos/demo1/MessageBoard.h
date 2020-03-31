#ifndef MESSAGEBOARD_H
#define MESSAGEBOARD_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>

#include "messageboardattachedtype.h"
#include "message.h"

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
