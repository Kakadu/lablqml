/*
 * Generated at 2013-11-06 23:46:53.840955+04:00
 */
#ifndef IntModel_c_H
#define IntModel_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>
#include <QtWidgets/QGraphicsSceneMouseEvent>
#include <QtGui/QKeyEvent>
#include <QtCore/QAbstractItemModel>

class IntModel: public QAbstractItemModel {
  Q_OBJECT
  value _camlobjHolder;
public:
  IntModel();
  void storeCAMLobj(value x) {
    if (_camlobjHolder != 0) {
       //maybe unregister global root?
    }
    _camlobjHolder = x;
    register_global_root(&_camlobjHolder);
  }
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
#endif // IntModel_H
