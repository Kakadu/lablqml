#include <QtCore/QObject>
#include <QtGui/QtGui>

class QWidget_twin : public QWidget {
  Q_OBJECT
public:
  QWidget_twin(QWidget* x0) : QWidget(x0,0) {}

  void virtual keyPressEvent(QKeyEvent *ev);
};
