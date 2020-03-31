#ifndef MYQMLTYPE_H
#define MYQMLTYPE_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>

class MyQmlType : public QObject//, public QQmlParserStatus
{
    Q_OBJECT
//    Q_INTERFACES(QQmlParserStatus)
    QML_ELEMENT
public:
    explicit MyQmlType(QObject *) {}

    virtual void componentComplete()
    {
        // Perform some initialization here now that the object is fully created
        qDebug() << Q_FUNC_INFO;
    }
};

#endif // MYQMLTYPE_H
