#include <QObject>

class MyDynamicQObject : public QObject {
  Q_OBJECT
public:
  void meth1();

public slots:
  //int  slot1();
  //void slot2(int);
signals:
  //void signal1();
};

