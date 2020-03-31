#ifndef MYINSTANTIABLE_H
#define MYINSTANTIABLE_H

#include <QtCore/QObject>
#include <QtQuick/QQuickItem>
#include <QtCore/QDateTime>

class MyInstantiable : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString author READ author WRITE setAuthor NOTIFY authorChanged)
    Q_PROPERTY(QDateTime creationDate READ creationDate WRITE setCreationDate NOTIFY creationDateChanged)
    QML_ELEMENT

public:
    explicit MyInstantiable(QObject *parent = nullptr);

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
