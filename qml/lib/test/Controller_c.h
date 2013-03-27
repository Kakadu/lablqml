/*
 * Generated at 2013-03-27 13:45:00.157469
 */
#ifndef Controller_c_H
#define Controller_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>

class Controller: public QObject {
  Q_OBJECT
public:
  Controller();
  Q_INVOKABLE void onItemSelected(int,int);
};
#endif // Controller_H
