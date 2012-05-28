
include mk/defaults.mk
include mk/build.mk

clean::
	$Q$(RM) -r build

build/bin:
	$(call cmd,mkdir)

# Projects
include src/dang.mk
