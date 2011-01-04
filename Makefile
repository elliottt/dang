
all:

include mk/verbose.mk
include mk/build.mk

ghci:
	$(call cmd,make_rec) -f dang.mk ghci

%:
	$(call cmd,make_rec) -f dang.mk $@
	$(call cmd,make_rec) -C rts $@
