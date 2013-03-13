#ifndef STUBS_H
#define STUBS_H

#include "kamlo.h"

#include <QtQuick/qquickview.h>
#include <QtCore/QDebug>
#include <QtQml/QQmlContext>

extern void registerContext(const QString& name, QQmlContext* v);
#endif
