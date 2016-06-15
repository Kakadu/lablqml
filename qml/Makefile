#compile all the tests
TEST_APPS=helloworld obj_test \
	propertyMap_test propmap_stress_test prop_test \
	signal_test threads_test \
	test

.PHONY: all clean
all:

define MAKE_RULES
.PHONY: make_$(1)
make_$(1):
	if [ -e $(1)/configure ]; then \
		cd $(1) && PATH=`pwd`/../../src:$$$$PATH ./configure ; fi
	PATH=`pwd`/../src:$$$$PATH OCAMLPATH=`pwd`/../lablqml/_build/bundle \
		$(MAKE) -C $(1) VERBOSE=1
clean_$(1):
	$(MAKE) -C $(1) clean

all: make_$(1)
clean: clean_$(1)
endef
$(foreach i,$(TEST_APPS),$(eval $(call MAKE_RULES,$(i)) ) )
