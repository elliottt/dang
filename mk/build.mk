
GHC_DIR		= ghc
GHC		= ghc -hidir $(GHC_DIR) -odir $(GHC_DIR) -i$(GHC_DIR)
GHC_FLAGS	= -Wall
CFLAGS		= -Wall
CXXFLAGS	= -Wall

cmd_ghc_o_hs		= $(GHC) $(GHC_FLAGS) -c -o $@ $<
quiet_cmd_ghc_o_hs	= GHC     $@

cmd_ghc_ld		= $(GHC) -o $@ $^
quiet_cmd_ghc_ld	= LD      $@

%.hi : %.o ;

cmd_cc_o_c		= clang -emit-llvm $(CFLAGS) -o $@ -c $<
quiet_cmd_cc_o_c	= CC      $@

cmd_cxx_o_cxx		= clang -emit-llvm $(CXXFLAGS) -o $@ -c $<
quiet_cmd_cxx_o_cxx	= CXX     $@

cmd_ar			= llvm-ar
quiet_cmd_ar		= AR      $@
