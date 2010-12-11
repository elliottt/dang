
TOPDIR	:= ..

include $(TOPDIR)/mk/verbose.mk
include $(TOPDIR)/mk/build.mk
include $(TOPDIR)/mk/clean.mk

SOURCES	:= $(shell find . -name '*.cxx')
HEADERS	:= $(shell find . -name '*.h')
OBJECTS	:= $(SOURCES:.cxx=.o)

.PHONY: all
all: librts.a

librts.a: $(OBJECTS)
	$(call cmd,ar)

%.o: %.cxx
	$(call cmd,cxx_o_cxx)

-include $(CURDIR)/.depend

$(CURDIR)/.depend: $(SOURCES) $(HEADERS)
	$(Q) $(CXX) -M $^ > $@

clean:
	$(call cmd,clean) $(RM) -f librts.a $(OBJECTS)
