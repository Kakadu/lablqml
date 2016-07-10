#include <QtCore/QObject>
#include <caml/mlvalues.h>

class QSingleFunc : public QObject
{
  Q_OBJECT
  value _saved_callback;
public:
  QSingleFunc(value v);
  ~QSingleFunc();
  Q_INVOKABLE void run();

};
