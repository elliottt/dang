

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

cmd_alex_hs_x       = $(ALEX) $(ALEXFLAGS) -o $@ $<
quiet_cmd_alex_hs_x = ALEX    $@

define alex_target
$(GHC_DIR)/$1.hs : src/$1.x
	$(Q) mkdir -p $$(dir $$@)
	$$(call cmd,alex_hs_x)
endef

HAPPY      = happy
HAPPYFLAGS =

cmd_happy_hs_y       = $(HAPPY) $(HAPPYFLAGS) -o $@ $<
quiet_cmd_happy_hs_y = HAPPY   $@

define happy_target
$(GHC_DIR)/$1.hs: src/$1.y
	$(Q) mkdir -p $$(dir $$@)
	$$(call cmd,happy_hs_y)
endef

# Clang

CFLAGS           = -Wall
cmd_cc_o_c       = clang -emit-llvm $(CFLAGS) -o $@ -c $<
quiet_cmd_cc_o_c = CC      $@

CXXFLAGS            = -Wall
cmd_cxx_o_cxx       = clang -emit-llvm $(CXXFLAGS) -o $@ -c $<
quiet_cmd_cxx_o_cxx = CXX     $@


# AR

cmd_ar       = llvm-ar rcu $@ $^
quiet_cmd_ar = AR      $@


# Make

cmd_make_rec = $(MAKE) --no-print-directory
