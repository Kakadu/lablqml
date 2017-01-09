A problem arose, out of the kind that are somewhat hard to deal with. At the moment, it is not possible to inherit from some abstract Qt/C++ classes, due to pure virtual methods which aren't transportable easily to the OCaml runtime, which essentially forbids the use of such classes in OCaml and ruins the binding's transparency.

_Exempli gratia_, there is such an abstract class _Foo_ that it contains a method with the following signature:

    class Foo {
      void bar(QModelIndex&) = 0;
    }

The problem lies in the absence of possibility to create a wrapping OCaml `twin' class around this C++ one, which is the approach used in the current version of lablQt (that is, a pointer to a C++ object is received from the C++ runtime, wrapped around with an OCaml class and juggled inside the OCaml runtime as needed), seeing as the compiler barfs at the twin class being abstract. _I. e._ we know of no way to transfer a value from the C++ stack to the OCaml runtime, this denies the possibility of inheriting such classes from OCaml ones.

Any helpful feedback on the problem is very much welcome.


This is description how inheritance for C++ widgets should work in OCaml.
Below we deal with ``protected method void QWidget::keyPressEvent(QKeyEvent*)``.

What we want from inheritance mechanism?

1. If method is not overriden, everything will work as without inheritance.
2. If method is overriden in OCaml, methods from C++ should know about it.

Take a look at simplified class QWidget:

    class QWidget {
    protected:
      virtual void keyPressEvent(QKeyEvent*);
    };

Let's create QWidget_twin class for overriding method ``keyPressEvent``.

    class QWidget_twin: public QWidget {
    public:
       virtual void keyPressEvent(QKeyEvent* ) { ... }
    };
Also we will add method ``call_super_keyPressEvent`` for executing default realisation:

    virtual void call_super_keyPressEvent(QKeyEvent * ev) {
      QWidget::keyPressEvent(ev);
    } 
OCaml values ``stub_keyPressEvent: qWidget -> qKeyEvent -> unit`` and ``stub_super_keyPressEvent:qWidget -> qKeyEvent -> unit`` will execute methods ``QWidget_twin::keyPressEvent`` and ``QWidget_twin::call_super_keyPressEvent`` accordingly.

Now we will define base class in OCaml:

    class qWidget me = object
      method handler = me
      method keyPressEvent e = stub_super_KeyPressEvent me e 
    end
We use such body of keyPressEvent because default implementation in OCaml should execute default implementation in C++.
Now define another class

    class mywidget me = object
      inherit qWidget me as super
      method handler = me
      method keyPressEvent e =  print_endline "inerited realisation"
    end

The last step is to explain C++ to check if method is inherited in OCaml. If true than call OCaml implementation else execute default implementation.

    class QWidget_twin : public QWidget {
    public:
      virtual void keyPressEvent(QKeyEvent*) {
        // grab OCaml object and execute method keyPressEvent from OCaml object
      }
    };

If method is overriden than new realisation will be executed else value ``stub_super_keyPressEvent`` will be executed and ``QWidget_twin::call_super_keyPressEvent`` after that.


## Aditional comments?
How to inherit abstract class in C++ (for example QAbstractButton). I think that best way is to make QAbstractButton_twin class where all pure virtual methods will be implemented by calling associated OCaml methods. In Ocaml abstract methods should be marked as ``virtual``. No other changes needed, I think.

P.S. Applouses for gds who has generated many ideas about implementing inheritance this way.

P.P.S. коммент от f[x]:
вариант на память - камлевый qwidget это внутри рекорд с кложурками, с сишной стороны это отнаследованный класс. все методы переопределены так что они сначала смотрят в табличку кложурок и дёргают их если есть
(от меня - прям как в qtHaskell)




