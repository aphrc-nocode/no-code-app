## No-code APP

## https://cygubicko.github.io/varpred/index.html 

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

autopipeR = defined

######################################################################

## Anything to be added is included in Sources += 

Sources += $(wildcard *.md)
Sources += $(wildcard *.R R/*.R)
Sources += $(wildcard js/*.js)
Sources += $(wildcard server/*.R)
Sources += $(wildcard ui/*.R)
Sources += $(wildcard static_files/*.*)
Sources += LICENSE

######################################################################

## All make rules goes here 

runapp:
	echo "shiny::runApp()" | R --slave

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/chains.mk
-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
