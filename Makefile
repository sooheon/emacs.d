.PHONY: all help build quick bootstrap upgrade
.FORCE:

BASEDIR := $(shell pwd)

all: build

help:
	$(info )
	$(info make [all|build]    = rebuild all drones)
	$(info make quick          = rebuild most drones)
	$(info make lib/DRONE      = rebuild DRONE)
	$(info make bootstrap      = bootstrap collective)
	@printf "\n"

build:
	@rm -f init.elc
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild

quick:
	@rm -f init.elc
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--eval  '(borg-batch-rebuild t)'

lib/%: .FORCE
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--eval  '(borg-build "$(@F)")'

bootstrap:
	git submodule init
	./borg-bootstrap
	make

upgrade: bootstrap
	cd $(BASEDIR) && $(emacs) -batch -l packages.el 2>&1 | tee -a etc/log
