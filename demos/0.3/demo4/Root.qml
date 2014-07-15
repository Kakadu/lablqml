import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    Component.onCompleted: {
      // get object from OCaml as method's result
      var o = controller.getobj();
      console.log(o);
      // use some methods
      console.log(o.getname());
      console.log(o.getage() );

      // We pass three values to OCaml side
      // N.B. method is the same
      controller.setobj(o);
      controller.setobj("asdf");
      controller.setobj(15);

      // get object from OCaml as property's value
      // to get a property's value we don't need paretheses at the end
      var p = controller.person;
      console.log(p);
      console.log(p.getname())
      console.log(p.getage());
    }
}
