
TOPDIR	:= ..

.PHONY: all
all: librts.a

include $(TOPDIR)/mk/verbose.mk
include $(TOPDIR)/mk/build.mk
include $(TOPDIR)/mk/clean.mk

CSOURCES := $(shell find . -name '*.c')
CHEADERS := $(shell find . -name '*.h')
SOURCES  := $(CSOURCES) gc-prim.ll
OBJECTS  := $(patsubst %.ll,%.o,$(patsubst %.c,%.o,$(SOURCES)))

librts.a: $(OBJECTS)
	$(call cmd,ar)

-include $(CURDIR)/.depend

$(CURDIR)/.depend: $(CSOURCES) $(CHEADERS)
	$(Q) $(CC) -M $^ > $@

clean:
	$(call cmd,clean) $(RM) -f librts.a $(OBJECTS) gc-prim.{bc,s}
