
.PHONY: all
all:

.PHONY: clean
clean::
	$Q$(RM) -r build

.PHONY: mrproper
mrproper:: clean

include mk/build.mk

build/bin:
	$(call cmd,mkdir)

include src/dang.mk
