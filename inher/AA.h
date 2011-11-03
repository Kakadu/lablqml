#include <QtCore/QObject>
#include <QtGui/QtGui>

class QWidget_twin : public QWidget {
  Q_OBJECT
public:
  void foo(int);
  QWidget_twin(QWidget* x0) : QWidget(x0,0) {}
  ~QWidget_twin() {}
  void virtual keyPressEvent(QKeyEvent *ev);
  void virtual call_super_keyPressEvent(QKeyEvent *ev);
};
