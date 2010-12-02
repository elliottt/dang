
ifeq ($(V),)
	cmd	= @echo -e "  $1\t$@"; $($1)
	Q	= @
else
	cmd	= $1
	Q	=
endif
