
TOPDIR := .

TARGET := dang

all: $(TARGET)

include mk/verbose.mk
include mk/build.mk
include mk/clean.mk

GHCFLAGS += -fspec-constr-count=2

LIBS := base monadLib llvm-pretty pretty containers GraphSCC bytestring \
	text cereal filepath directory process

HAPPY_MODS := Syntax/Parser
ALEX_MODS  := Syntax/Lexer
SLASH_MODS := \
    CodeGen \
    Colors \
    Compile \
    Compile/LambdaLift \
    Compile/Rename \
    Core/AST \
    Dang/FileName \
    Dang/IO \
    Dang/Monad \
    Dang/Tool \
    Data/ClashMap \
    Interface \
    Link \
    Main \
    ModuleSystem \
    ModuleSystem/Imports \
    ModuleSystem/Resolve \
    Pretty \
    Prim \
    QualName \
    ReadWrite \
    Syntax \
    Syntax/AST \
    Syntax/ParserCore \
    TypeChecker \
    TypeChecker/CheckKinds \
    TypeChecker/CheckTypes \
    TypeChecker/Env \
    TypeChecker/Monad \
    TypeChecker/Types \
    TypeChecker/Unify \
    Utils \
    Variables

HS_SOURCES := $(addprefix src/,$(addsuffix .hs,$(SLASH_MODS))) \
              $(addprefix ghc/,$(addsuffix .hs,$(ALEX_MODS) $(HAPPY_MODS)))
HS_OBJECTS := $(addprefix $(GHC_DIR)/, \
                $(addsuffix .o,$(SLASH_MODS) $(ALEX_MODS) $(HAPPY_MODS)))
HS_LIBS    := $(addprefix -package ,$(LIBS))

$(eval $(foreach mod,$(ALEX_MODS),$(call alex_target,$(mod))))
$(eval $(foreach mod,$(HAPPY_MODS),$(call happy_target,$(mod))))

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

print-%:
	@echo "$* = $($*)"
