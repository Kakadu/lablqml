#include <stdio.h>

class A {
public:
	A();
	void foo() { 
                printf ("inside A::foo\n"); fflush(stdout);
                boo();
		printf ("exit from A::foo\n"); fflush(stdout);

	}
	void virtual boo() = 0;
};

