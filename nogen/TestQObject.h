#include <QtCore/QObject>

class TestQObject : public QObject {
  Q_OBJECT
public:
  Q_INVOKABLE void meth1() {}
public slots:
  void slot1() {}
signals:
  void signal1();
};

