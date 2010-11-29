AR	= llvm-ar
CC	= clang
CFLAGS	= -emit-llvm -Wall

ifeq ($V,)
	QUIET_CC    = @echo "  CC    $@";
	QUIET_AR    = @echo "  AR    $@";
	QUIET_CLEAN = @echo "  CLEAN $(CURDIR)";
	Q           = @
else
	QUIET_CC    =
	QUIET_AR    =
	QUIET_CLEAN =
	Q           =
endif

SOURCES	= rts.c rts-main.c gc.c
OBJECTS	= $(SOURCES:.c=.o)

.PHONY: all
all: librts.a

librts.a: $(OBJECTS)
	$(QUIET_AR) $(AR) rcu $@ $^

%.o: %.c
	$(QUIET_CC) $(CC) $(CFLAGS) -c $< -o $@

clean:
	$(QUIET_CLEAN)
	$(Q)$(RM) -f librts.a $(OBJECTS)
