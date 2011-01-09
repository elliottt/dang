
TOPDIR = $(CURDIR)/..

DANG = $(TOPDIR)/dang

test: Main.o Id.o Fix.o
	llvm-ld -o $@ $^ $(TOPDIR)/rts/librts.a

test-debug: Main.o Id.o Fix.o
	llvm-ld -o $@ $^ $(TOPDIR)/rts/librts.a -disable-opt
	llc -o $@.s $@.bc
	gcc -o $@ -g $@.s

Main.o: Id.o
Main.o: Fix.o

%.o: %.dg
	$(DANG) -c $<

clean:
	$(RM) *.o *.di test test.bc
