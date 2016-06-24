#include <QtCore/QObject>
#include <caml/mlvalues.h>

class QSingleFunc : public QObject
{
  Q_OBJECT
  value _caml_callback;
public:
  QSingleFunc(value v);
  ~QSingleFunc() {
      // TODO: unregister global root
  }
  Q_INVOKABLE void run();

};
