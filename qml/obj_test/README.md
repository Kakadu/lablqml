Demo application with ppx\_qt syntax extension. 

Startup from OCaml. An object is passed to the QML side. In QML we call a method with different arguments.
This method has 1 argument of type QVariant (it is not necessary that all methods should have arguments 
wrapped into QVariant, this is a demo for this kind of methods). On OCaml-side runs different code for
different argument type)

If you are curious wrapping arguments to the QVariant allows to pass QObject to the OCaml without any
preceding compile-time codegeneration. But you will need to cast it unsafely to what you want. 

