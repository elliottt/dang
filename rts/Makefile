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

SOURCES	= rts.cxx rts-main.cxx gc.cxx
OBJECTS	= $(SOURCES:.cxx=.o)

.PHONY: all
all: librts.a

librts.a: $(OBJECTS)
	$(call cmd,AR) rcu $@ $^

%.o: %.cxx
	$(call cmd,CXX) $(CFLAGS) -c $< -o $@

clean:
	$(Q)$(RM) -f librts.a $(OBJECTS)
