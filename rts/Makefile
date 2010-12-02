
TOPDIR	:= ..

include $(TOPDIR)/mk/verbose.mk

AR		= llvm-ar
CXX		= clang
CXXFLAGS	= -emit-llvm -Wall

SOURCES	:= $(shell find . -name '*.cxx')
HEADERS	:= $(shell find . -name '*.h')
OBJECTS	:= $(SOURCES:.cxx=.o)

.PHONY: all
all: librts.a

librts.a: $(OBJECTS)
	$(call cmd,AR) rcu $@ $^

%.o: %.cxx
	$(call cmd,CXX) $(CXXFLAGS) -c $< -o $@

-include .depend

.depend: $(SOURCES) $(HEADERS)
	$(Q) $(CXX) -M $^ > $@

clean:
	$(Q)$(RM) -f librts.a $(OBJECTS)
