

# GHC

GHC_DIR := ghc

$(GHC_DIR):
	$(Q) mkdir -p $@

GHC       = ghc -hidir $(GHC_DIR) -odir $(GHC_DIR) -i$(GHC_DIR)
GHC_FLAGS = -Wall

cmd_ghci = $(GHC) --interactive -v0

quiet_cmd_ghc_o_hs = GHC     $@
      cmd_ghc_o_hs = $(GHC) $(GHC_FLAGS) -c $<

%.o : %.hs
	$(call cmd,ghc_o_hs)

quiet_cmd_ghc_ld = LD      $@
      cmd_ghc_ld = $(GHC) -o $@ $^

%.hi : %.o ;

ALEX      = alex
ALEXFLAGS =

quiet_cmd_alex_hs_x = ALEX    $@
      cmd_alex_hs_x = $(ALEX) $(ALEXFLAGS) -i -o $@ $<

define alex_target
$(GHC_DIR)/$1.hs : src/$1.x
	$(Q) mkdir -p $$(dir $$@)
	$$(call cmd,alex_hs_x)
endef

HAPPY      = happy
HAPPYFLAGS =

quiet_cmd_happy_hs_y = HAPPY   $@
      cmd_happy_hs_y = $(HAPPY) $(HAPPYFLAGS) -i -o $@ $<

define happy_target
$(GHC_DIR)/$1.hs: src/$1.y
	$(Q) mkdir -p $$(dir $$@)
	$$(call cmd,happy_hs_y)
endef


# C Compilation
CC               = gcc
CFLAGS           = -Wall
quiet_cmd_cc_o_c = CC      $@
      cmd_cc_o_c = $(CC) $(CFLAGS) -o $@ -c $<


%.o: %.c
	$(call cmd,cc_o_c)

quiet_cmd_cc_o_bc = CC[BC]  $@
      cmd_cc_o_bc = $(CC) $(CFLAGS) -emit-llvm -o $@ -c $<

%.bc: %.c
	$(call cmd,cc_o_bc)


# Assembler
AS               = as
ASFLAGS          =
quiet_cmd_as_o_s = AS      $@
      cmd_as_o_s = $(AS) $(ASFLAGS) -o $@ $<

%.o: %.s
	$(call cmd,as_o_s)


# LLVM Assembler
LLVM_AS         = llvm-as
LLVM_ASFLAGS    =
quiet_cmd_ll_bc = LLVM-AS $@
      cmd_ll_bc = $(LLVM_AS) $(LLVM_ASFLAGS) -o $@ $<

%.bc: %.ll
	$(call cmd,ll_bc)


# Preprocessed LLVM Assembly
quiet_cmd_ll_in = CPP     $@
      cmd_ll_in = $(CPP) -DASSEMBLY -P -o $@ $<

%.ll: %.ll.S
	$(call cmd,ll_in)

# Native Assembly Generation
LLC            = llc
LLCFLAGS       =
quiet_cmd_bc_s =
      cmd_bc_s = $(LLC) $(LLCFLAGS) -o $@ $<

%.s: %.bc
	$(call cmd,bc_s)


# AR
AR           = ar
RANLIB       = ranlib
cmd_ar       = $(AR) rcu $@ $^; $(RANLIB) $@
quiet_cmd_ar = AR      $@


# Make
cmd_make_rec = $(MAKE) --no-print-directory
