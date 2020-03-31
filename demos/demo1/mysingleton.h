#ifndef MYSINGLETON_H
#define MYSINGLETON_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>

class MySingleton : public QObject
{
    Q_OBJECT
    QML_NAMED_ELEMENT(MyApi)
    QML_SINGLETON
    Q_PROPERTY(int someProperty READ someProperty WRITE setSomeProperty NOTIFY somePropertyChanged)
public:
    explicit MySingleton(QObject *parent = nullptr);

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
