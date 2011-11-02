#include <QtCore/QObject>
#include <QtGui/QtGui>

class QWidget_twin : public QWidget {
  Q_OBJECT
public:
  QWidget_twin(QWidget* x0) : QWidget(x0,0) {}
  ~QWidget_twin() {}
  bool foo() { return false; }
  void virtual keyPressEvent(QKeyEvent *ev);
  void virtual call_super_keyPressEvent(QKeyEvent *ev) {
    if (foo())
      foo();
    QWidget::keyPressEvent(ev);
  }
};
