
# Command Infrastructure #######################################################

ifneq ($(V),1)
Q     := @
quiet := quiet_
else
Q     :=
quiet :=
endif

cmd = $(if $($(quiet)cmd_$1),@echo '  $($(quiet)cmd_$1)';)$(cmd_$1)


# GHC ##########################################################################

GHC      = ghc
GHCFLAGS =

quiet_cmd_hs_to_o = HC      $@
      cmd_hs_to_o = $(GHC) $(GHCFLAGS) -c $< -o $@

%.hi: %.o ;

%.o: %.hs
	$(call cmd,hs_to_o)

quiet_cmd_link_hs = LD      $@
      cmd_link_hs = $(GHC) $(LDFLAGS) $(OBJECTS) -o $@

quiet_cmd_hs_depend = DEP     $@
      cmd_hs_depend = $(GHC) -dep-makefile $@ -M $(GHCFLAGS) $(SOURCES)


# Happy ########################################################################

HAPPY      = happy
HAPPYFLAGS =

quiet_cmd_y_to_hs = HAPPY   $@
      cmd_y_to_hs = $(HAPPY) $(HAPPYFLAGS) -o $@ $<

%.hs: %.y
	$(call cmd,y_to_hs)


# Alex #########################################################################

ALEX      = alex
ALEXFLAGS =

quiet_cmd_x_to_hs = ALEX    $@
      cmd_x_to_hs = $(ALEX) $(ALEXFLAGS) -o $@ $<

%.hs: %.x
	$(call cmd,x_to_hs)
