
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

quiet_cmd_hs_to_o = GHC      $(notdir $@)
      cmd_hs_to_o = $(GHC) $(GHCFLAGS) -c $< -o $@

%.hi: %.o ;

%.o: %.hs
	$(call cmd,hs_to_o)


# Happy ########################################################################

HAPPY      = happy
HAPPYFLAGS =

quiet_cmd_y_to_hs = HAPPY    $(notdir $@)
      cmd_y_to_hs = $(HAPPY) $(HAPPYFLAGS) -o $@ $<

%.hs: %.y
	$(call cmd,y_to_hs)


# Alex #########################################################################

ALEX      = alex
ALEXFLAGS =

quiet_cmd_x_to_hs = ALEX    $(notdir $@)
      cmd_x_to_hs = $(ALEX) $(ALEXFLAGS) -o $@ $<
