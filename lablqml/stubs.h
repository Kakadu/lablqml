#ifndef STUBS_H
#define STUBS_H

#include "kamlo.h"

#include <QtCore/QAbstractItemModel>
#include <QtCore/QString>
#include <QtQml/QQmlContext>

extern void registerContext(const QString& name, QQmlContext* v);
extern QModelIndex make_qmodelindex4(int, int, void*, const QAbstractItemModel *);

#endif // STUBS_H
