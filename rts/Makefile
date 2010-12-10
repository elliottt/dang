
TOPDIR	:= ..

include $(TOPDIR)/mk/verbose.mk
include $(TOPDIR)/mk/build.mk
include $(TOPDIR)/mk/clean.mk

AR		= llvm-ar
CXX		= clang
CXXFLAGS	= -emit-llvm -Wall -g

SOURCES	:= $(shell find . -name '*.cxx')
HEADERS	:= $(shell find . -name '*.h')
OBJECTS	:= $(SOURCES:.cxx=.o)

.PHONY: all
all: librts.a

librts.a: $(OBJECTS)
	$(call cmd,ar) rcu $@ $^

%.o: %.cxx
	$(call cmd,cxx_o_cxx)

-include .depend

.depend: $(SOURCES) $(HEADERS)
	$(Q) $(CXX) -M $^ > $@

clean:
	$(Q)$(RM) -f librts.a $(OBJECTS)
