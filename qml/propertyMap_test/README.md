Demo with [QQmlProperyMap](http://doc.qt.io/qt-5/qqmlpropertymap.html).

Use `make` and `./propMapTest.native` to run the demo. This code will be 
probably merged to the lablqml library.

At the moment we support changing properties on OCaml side and signalizing 
about it in QML. Th reverse behaviour is not yet implemented: we need to
specifiy callback in `PropMap.create` function and connect it to the
signal `valueChanged(const QString & key, const QVariant & value)`. 
Probably we need to 
[connect the signal to the lambda](`https://wiki.qt.io/New_Signal_Slot_Syntax).


