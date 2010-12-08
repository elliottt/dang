
TOPDIR	:= .

include mk/verbose.mk
include mk/build.mk
include mk/clean.mk

SLASH_MODS	:= $(subst src/,,$(basename $(shell find src -name '*.hs')))
LIBS		:= base monadLib llvm-pretty pretty containers GraphSCC

HS_SOURCES	:= $(addprefix src/,$(addsuffix .hs,$(SLASH_MODS)))
HS_OBJECTS	:= $(addprefix $(GHC_DIR)/,$(addsuffix .o,$(SLASH_MODS)))
HS_LIBS		:= $(addprefix -package ,$(LIBS))

TARGET		:= test

all: $(TARGET) rts

rts:
	$(MAKE) -C rts all

$(TARGET): $(HS_OBJECTS)
	$(call cmd,ghc_ld) $(HS_LIBS)

$(GHC_DIR):
	$(Q) mkdir $(GHC_DIR)

-include $(GHC_DIR)/depend

$(GHC_DIR)/depend: $(GHC_DIR)
	$(Q) $(GHC) -M -dep-makefile $@ $(HS_SOURCES)

$(GHC_DIR)/%.o: src/%.hs
	$(call cmd,ghc_o_hs)

clean:
	$(call cmd,RM) -r ghc
	$(call cmd,RM) $(TARGET)
