import QtQuick 2.0

Item {
    width: 400 + controller.x
    height: 600 + controller.y
    visible: true

    Component.onCompleted: {
      var o = controller.getobj();
      console.log(o);
      console.log(o.getname());
      console.log(o.getage() );

      controller.setobj(o);
      controller.setobj("asdf");
      controller.setobj(15);

      var p = controller.person;
      console.log(p);
      console.log(p.getname())
      console.log(p.getage());
    }
}
