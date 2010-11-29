
ifeq ($(V),)
	Q	= @
	Q_GHC	= @echo "  GHC    $@ ";
else
	Q	=
	Q_GHC	=
endif

GHC_DIR		:= ghc
GHC		= ghc -hidir $(GHC_DIR) -odir $(GHC_DIR) -i$(GHC_DIR)
GHC_FLAGS	= -Wall

SLASH_MODS	:= $(subst src/,,$(basename $(shell find src -name '*.hs')))
LIBS		:= base monadLib llvm-pretty pretty containers GraphSCC

HS_SOURCES	:= $(addprefix src/,$(addsuffix .hs,$(SLASH_MODS)))
HS_OBJECTS	:= $(addprefix $(GHC_DIR)/,$(addsuffix .o,$(SLASH_MODS)))
HS_LIBS		:= $(addprefix -package ,$(LIBS))

TARGET		:= test

all: $(TARGET)

$(TARGET): $(HS_OBJECTS)
	$(GHC) $(GHC_FLAGS) -o $@ $(HS_LIBS) $(HS_OBJECTS)

$(GHC_DIR):
	$(Q) mkdir $(GHC_DIR)

-include $(GHC_DIR)/depend

$(GHC_DIR)/depend: $(GHC_DIR)
	$(Q) $(GHC) -M -dep-makefile $@ $(HS_SOURCES)

%.hi: %.o ;

$(GHC_DIR)/%.o: src/%.hs
	$(Q_GHC) $(GHC) $(GHC_FLAGS) -c $< -o $@

clean:
	$(Q) $(RM) -r ghc
	$(Q) $(RM) $(TARGET)
