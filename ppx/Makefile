.PHONY: celan
DUNEOPTS=

ifeq ($(VERBOSE),1)
V=
DUNEOPTS += --verbose
else
V=@
endif


all:
	$(V)dune build pp_qt.exe $(DUNEOPTS)

celan:	clean
clean:	
	$(V)$(RM) -r ../_build/default/ppx

ppx_qt: ppx_qt.native
	ln -sf ./ppx_qt.native ./ppx_qt


