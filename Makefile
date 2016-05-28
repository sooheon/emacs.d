.PHONY: all help build quick pull upgrade up
.FORCE:

emacs ?= emacs

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
	@emacs --batch -L lib/borg --load borg \
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

pull:
	git pull
	git submodule init
	git submodule update

upgrade: pull
	cd $(BASEDIR) && $(emacs) -batch -l packages.el

up: upgrade
	$(emacs) -Q -l init.el
