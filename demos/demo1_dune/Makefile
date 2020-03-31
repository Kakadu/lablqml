.PHONY: celan

OUT=../dune_test/program.exe
OUT=./program.exe
DUNEOPTS=

ifeq ($(VERBOSE),1)
V=
DUNEOPTS += --verbose
else
V=@
endif

DUNEOPTS+= --root .

all:
	$(V)dune build $(OUT) $(DUNEOPTS)

run:
	$(V)dune exec  $(OUT) $(DUNEOPTS)

celan: clean
clean:
	$(RM) -r ../_build/default/dune_test
	$(RM) -r _build


