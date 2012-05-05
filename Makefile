
.PHONY: all
all:

.PHONY: clean
clean::

.PHONY: mrproper
mrproper:: clean

include mk/build.mk

include dang.mk
