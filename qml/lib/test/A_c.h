/*
 * Generated at 2013-03-16 00:32:51.303402
 */
#ifndef A_c_H
#define A_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>
#include <QtCore/QAbstractItemModel>

class A: public QAbstractItemModel {
  Q_OBJECT
  value camlobj = 0;
public:
  A(value _camlobj);
  Q_INVOKABLE int sizey();
  Q_INVOKABLE QModelIndex parent(const QModelIndex &) const;
  Q_INVOKABLE QModelIndex index(int,int,const QModelIndex &) const;
  Q_INVOKABLE int columnCount(const QModelIndex &) const;
  Q_INVOKABLE int rowCount(const QModelIndex &) const;
  Q_INVOKABLE bool hasChildren(const QModelIndex &) const;
  Q_INVOKABLE QVariant data(const QModelIndex &,int) const;
private:
  QHash<int, QByteArray> _roles;
public:
Q_INVOKABLE QList<QString> roles() {
  QList<QString> ans;
  foreach(QByteArray b, _roles.values() )
      ans << QString(b);
  return ans;
}
void addRole(int r,QByteArray name) { _roles.insert(r,name); }
virtual QHash<int, QByteArray> roleNames() const { return _roles; }
};
#endif // A_H
