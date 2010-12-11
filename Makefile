
TOPDIR := .

TARGET := test

all: $(TARGET) librts.a

include mk/verbose.mk
include mk/build.mk
include mk/clean.mk

LIBS := base monadLib llvm-pretty pretty containers GraphSCC bytestring \
	utf8-string

HAPPY_MODS := $(subst src/,,$(basename $(shell find src -name '*.y')))
ALEX_MODS  := $(subst src/,,$(basename $(shell find src -name '*.x')))
SLASH_MODS := $(subst src/,,$(basename $(shell find src -name '*.hs')))

HS_SOURCES := $(addprefix src/,$(addsuffix .hs,$(SLASH_MODS))) \
              $(addprefix ghc/,$(addsuffix .hs,$(ALEX_MODS) $(HAPPY_MODS)))
HS_OBJECTS := $(addprefix $(GHC_DIR)/, \
                $(addsuffix .o,$(SLASH_MODS) $(ALEX_MODS) $(HAPPY_MODS)))
HS_LIBS    := $(addprefix -package ,$(LIBS))

$(eval $(foreach mod,$(ALEX_MODS),$(call alex_target,$(mod))))
$(eval $(foreach mod,$(HAPPY_MODS),$(call happy_target,$(mod))))

librts.a:
	$(call cmd,make_rec) -C rts librts.a

$(TARGET): $(HS_OBJECTS)
	$(call cmd,ghc_ld) $(HS_LIBS)

ghci: $(HS_OBJECTS)
	$(call cmd,ghci) $(HS_LIBS) -isrc

ghc/%.o : src/%.hs
	$(call cmd,ghc_o_hs)

-include $(GHC_DIR)/depend

$(GHC_DIR)/depend: $(GHC_DIR) $(HS_SOURCES)
	$(Q) $(GHC) -M -dep-makefile $@ $(HS_SOURCES)

clean:
	$(call cmd,clean) -r ghc $(TARGET)
	$(call cmd,make_rec) -C rts clean
