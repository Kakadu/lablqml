#include <stdio.h>

class A {
public:
	A();
	void foo() { 
		printf ("inside A::foo\n");
	  	boo();
		printf ("exit from A::foo\n");

	}
	void virtual boo() = 0;
};

