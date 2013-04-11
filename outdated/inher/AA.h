#include <QtCore/QObject>
#include <QtGui/QtGui>

class QWidget_twin : public QSpinBox {
  Q_OBJECT
public:
  void foo(int);
  QWidget_twin(QWidget* x0) : QSpinBox(x0) {}
  ~QWidget_twin() {}
  void virtual acceptDrops();
  void virtual keyPressEvent(QKeyEvent *ev);
  void virtual call_super_keyPressEvent(QKeyEvent *ev);
};
