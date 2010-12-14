
all:

include mk/verbose.mk
include mk/build.mk

all:
	$(call cmd,make_rec) -f dang.mk all
	$(call cmd,make_rec) -C rts all

clean:
	$(call cmd,make_rec) -f dang.mk clean
	$(call cmd,make_rec) -C rts clean
