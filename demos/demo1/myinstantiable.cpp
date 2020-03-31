#include "myinstantiable.h"

MyInstantiable::MyInstantiable(QObject *parent) : QObject(parent)
{
    Q_UNUSED(parent);
}
