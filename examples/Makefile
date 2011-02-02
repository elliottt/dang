
TOPDIR = $(CURDIR)/..

DANG = $(TOPDIR)/dang
RTS  = $(TOPDIR)/rts/librts.a

test: Main.o Id.o Fix.o
	gcc -o $@ $^ $(RTS)

Main.o: Id.o
Main.o: Fix.o

%.o: %.dg
	$(DANG) -c $<

clean:
	$(RM) *.o *.di
	$(RM) test
