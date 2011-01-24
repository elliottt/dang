

# GHC

GHC_DIR := ghc

$(GHC_DIR):
	$(Q) mkdir -p $@

GHC       = ghc -hidir $(GHC_DIR) -odir $(GHC_DIR) -i$(GHC_DIR)
GHC_FLAGS = -Wall

cmd_ghci = $(GHC) --interactive -v0

cmd_ghc_o_hs       = $(GHC) $(GHC_FLAGS) -c $<
quiet_cmd_ghc_o_hs = GHC     $@

%.o : %.hs
	$(call cmd,ghc_o_hs)

cmd_ghc_ld         = $(GHC) -o $@ $^
quiet_cmd_ghc_ld   = LD      $@

%.hi : %.o ;

ALEX      = alex
ALEXFLAGS =

cmd_alex_hs_x       = $(ALEX) $(ALEXFLAGS) -i -o $@ $<
quiet_cmd_alex_hs_x = ALEX    $@

define alex_target
$(GHC_DIR)/$1.hs : src/$1.x
	$(Q) mkdir -p $$(dir $$@)
	$$(call cmd,alex_hs_x)
endef

HAPPY      = happy
HAPPYFLAGS =

cmd_happy_hs_y       = $(HAPPY) $(HAPPYFLAGS) -i -o $@ $<
quiet_cmd_happy_hs_y = HAPPY   $@

define happy_target
$(GHC_DIR)/$1.hs: src/$1.y
	$(Q) mkdir -p $$(dir $$@)
	$$(call cmd,happy_hs_y)
endef

# Clang

CC               = clang
CFLAGS           = -Wall
cmd_cc_o_c       = $(CC) $(CFLAGS) -o $@ -c $<
quiet_cmd_cc_o_c = CC      $@


%.o: %.c
	$(call cmd,cc_o_c)

cmd_cc_o_bc       = $(CC) $(CFLAGS) -emit-llvm -o $@ -c $<
quiet_cmd_cc_o_bc = CC[BC]  $@

%.bc: %.c
	$(call cmd,cc_o_bc)


# LLVM Assembler
LLVM_AS           = llvm-as
LLVM_ASFLAGS      =
cmd_ll_o_bc       = $(LLVM_AS) $(LLVM_ASFLAGS) -o $@ $<
quiet_cmd_ll_o_bc = AS      $@

%.bc: %.ll
	$(call cmd,ll_o_bc)


LLC              = llc
LLCFLAGS         =
cmd_bc_o_s       = $(LLC) $(LLCFLAGS) -o $@ $<
quiet_cmd_bc_o_s = LLC     $@

%.s: %.ll
	$(call cmd,ll_o_bc)
	$(call cmd,bc_o_s)


# AR

AR           = llvm-ar
RANLIB       = llvm-ranlib
cmd_ar       = $(AR) rcu $@ $^; $(RANLIB) $@
quiet_cmd_ar = AR      $@


# Make

cmd_make_rec = $(MAKE) --no-print-directory
