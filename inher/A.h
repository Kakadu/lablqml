class A {
	public:
		A();
		int foo() { return boo() +1 ; }
		int virtual boo() = 0;
};

