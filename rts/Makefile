
TOPDIR	:= ..

.PHONY: all
all: librts.a

include $(TOPDIR)/mk/verbose.mk
include $(TOPDIR)/mk/build.mk
include $(TOPDIR)/mk/clean.mk
include $(TOPDIR)/mk/print.mk

CSOURCES := $(shell find . -name '*.c')
CHEADERS := $(shell find . -name '*.h')
SOURCES  := $(CSOURCES) gc-prim.ll apply.ll.S
OBJECTS  := $(patsubst %.ll,%.o, \
            $(patsubst %.c,%.o, \
            $(patsubst %.ll.S,%.o,$(SOURCES))))
CFLAGS += -g

librts.a: $(OBJECTS)
	$(call cmd,ar)

-include $(CURDIR)/.depend

$(CURDIR)/.depend: $(CSOURCES) $(CHEADERS)
	$(Q) $(CC) -M $^ > $@

clean:
	$(call cmd,clean) $(RM) -f librts.a $(OBJECTS) gc-prim.{bc,s}
