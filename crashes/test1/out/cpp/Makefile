INCLUDES=-I./../../ -I. `pkg-config --cflags QtOpenGL`  -I/usr/include/qt4
GCC=g++ -c -pipe -g -Wall -W $(INCLUDES) 

HEADERS=QWidget_twin.h
HEADERS2=$(addprefix moc_,$(HEADERS))
MOCS=$(HEADERS2:.h=.o)
C_QTOBJS=QWidget_twin.o QKeyEvent.o QObject.o QWidget.o Qt/enum_Qt_WindowFlags.o

.SUFFIXES: .h .cpp .o

all: step1 step2

step2: $(MOCS)

step1: $(C_QTOBJS) $(HEADERS)

moc_%.cpp: %.h
		moc $< > $@

.cpp.o:
	$(GCC) -c -I`ocamlc -where` -I.. $(COPTS) -fpic $<

.cpp.h:
	moc $@ > moc_$<

clean: 
	rm -f *.o

.PHONY: all clean

