AR		= llvm-ar
CXX		= clang
CXXFLAGS	= -emit-llvm -Wall

ifeq ($V,)
	cmd	= @echo -e "  $1\t$@ "; $($1)
	Q	= @
else
	cmd	= $($1)
	Q	=
endif

SOURCES	= rts.c rts-main.c gc.c
OBJECTS	= $(SOURCES:.c=.o)

.PHONY: all
all: librts.a

librts.a: $(OBJECTS)
	$(call cmd,AR) rcu $@ $^

%.o: %.c
	$(call cmd,CXX) $(CFLAGS) -c $< -o $@

clean:
	$(Q)$(RM) -f librts.a $(OBJECTS)
