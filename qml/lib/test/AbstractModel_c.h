/*
 * Generated at 2013-03-29 15:40:45.382907
 */
#ifndef AbstractModel_c_H
#define AbstractModel_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>
#include <QtCore/QAbstractItemModel>

class AbstractModel: public QAbstractItemModel {
  Q_OBJECT
public:
  AbstractModel();
  Q_INVOKABLE QModelIndex parent(const QModelIndex &) const;
  Q_INVOKABLE QModelIndex index(int,int,const QModelIndex &) const;
  Q_INVOKABLE int columnCount(const QModelIndex &) const;
  Q_INVOKABLE int rowCount(const QModelIndex &) const;
  Q_INVOKABLE bool hasChildren(const QModelIndex &) const;
  Q_INVOKABLE QVariant data(const QModelIndex &,int) const;
private:
  QHash<int, QByteArray> _roles;
public:
  QModelIndex makeIndex(int row,int column) {
    if (row==-1 || column==-1)
      return QModelIndex();
    else
      return createIndex(row,column,(void*)NULL);
  }
Q_INVOKABLE QList<QString> roles() {
  QList<QString> ans;
  foreach(QByteArray b, _roles.values() )
      ans << QString(b);
  return ans;
}
void addRole(int r, QByteArray name) { _roles.insert(r,name); }
virtual QHash<int, QByteArray> roleNames() const { return _roles; }
void emit_dataChanged(int a, int b, int c, int d) {
  const QModelIndex topLeft     = createIndex(a,b);
  const QModelIndex bottomRight = createIndex(c,d);
  emit dataChanged(topLeft, bottomRight);
}
};
#endif // AbstractModel_H
