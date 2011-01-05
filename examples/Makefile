
TOPDIR = $(CURDIR)/..

DANG = $(TOPDIR)/dang

test: Main.o Id.o
	llvm-ld -o $@ $^ $(TOPDIR)/rts/librts.a

Main.o: Id.o

%.o: %.dg
	$(DANG) -c $<

clean:
	$(RM) *.o *.di test test.bc
