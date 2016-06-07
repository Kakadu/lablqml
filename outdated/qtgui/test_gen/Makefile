QT5=/home/kakadu/mand/prog/qt/qt5/qtbase
GCC=g++ -c -pipe -g -Wall -W -D_REENTRANT -I${QT5}/include
C_LIBS=-L${QT5}/lib -lQt5Widgets -lQt5Core
C_INC= -I${QT5}/include
CMOS=stub_helpers.cmo qtstubs.cmo
CMXS=stub_helpers.cmx qtstubs.cmx
C_STUBS=cstubs.o

BIND_CPP_OBJS=`find ./out/cpp/ -name "*.o"`
BIND_ML_OBJS=    out/stubs.cmo out/classes.cmo out/creators.cmo
BIND_ML_OBJS_OPT=out/stubs.cmx out/classes.cmx out/creators.cmx

.SUFFIXES: .cpp .ml .cmx .cmo

all: byte opt

byte: helpers_cmo kamlo $(C_STUBS) mklib

opt:  helpers_cmx kamlo $(C_STUBS) mklib_opt

helpers: helpers_cmx helpers_cmo

helpers_cmo: $(CMOS)

helpers_cmx: $(CMXS)

test:
	$(MAKE) -C test
	#$(MAKE) -C test4
	#$(MAKE) -C test5 depend all
	#$(MAKE) -C test6 depend all

kamlo:
	$(MAKE) -C out 

mklib:
	ocamlmklib -o lablqt $(C_LIBS) -lstdc++ -lpthread  $(C_STUBS) $(CMOS) \
		$(BIND_CPP_OBJS) $(BIND_ML_OBJS) 

mklib_opt:
	ocamlmklib -o lablqt $(C_LIBS) -lstdc++ -lpthread  $(C_STUBS) $(CMXS) \
		$(BIND_CPP_OBJS) $(BIND_ML_OBJS_OPT) 

.cpp.o:
	$(GCC) -c -I`ocamlc -where` $(C_INC) -fpic $<

.ml.cmx:
	ocamlopt -c $<

.ml.cmo:
	ocamlc -c $<

clean: clean_tests
	rm -f *.o *.a *.cm[iaox] *.cmxa dlllablqt.so 

clean_tests:
	$(MAKE) -C test  clean --no-print-directory
	$(MAKE) -C test4 clean --no-print-directory
	echo "" > test5/.depend
	$(MAKE) -C test5 clean --no-print-directory
	echo "" > test6/.depend
	$(MAKE) -C test6 clean --no-print-directory

