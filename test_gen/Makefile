GCC=g++ -c -pipe -g -Wall -W -D_REENTRANT `pkg-config --cflags QtOpenGL`

CMXS=stub_helpers.cmo qtstubs.cmo
C_STUBS=cstubs.o

BIND_CPP_OBJS=`find ./out/cpp/ -name "*.o"`
BIND_ML_OBJS=out/stubs.cmo out/classes.cmo out/creators.cmo

.SUFFIXES: .cpp .ml .cmx .cmo

all: $(CMXS) kamlo $(C_STUBS) mklib

test:
	$(MAKE) -C test
	$(MAKE) -C test2
	$(MAKE) -C test3
	$(MAKE) -C test4
	$(MAKE) -C test5 depend all


kamlo:
	$(MAKE) -C out 

mklib:
	ocamlmklib -o lablqt -L/usr/lib/qt4 -lQtGui -lQtCore -lQtOpenGL -lpthread  $(C_STUBS) $(CMXS) \
		$(BIND_CPP_OBJS) $(BIND_ML_OBJS) 


.cpp.o:
	$(GCC) -c -I`ocamlc -where` $(COPTS) -fpic $<

.ml.cmx:
	ocamlopt -c $<

.ml.cmo:
	ocamlc -c $<

clean:
	rm -f *.o *.a *.cm[iaox] dlllablqt.so 
	$(MAKE) -C test  clean
	$(MAKE) -C test2 clean
	$(MAKE) -C test3 clean
	$(MAKE) -C test4 clean
	echo "" > test5/.depend
	$(MAKE) -C test5 clean

