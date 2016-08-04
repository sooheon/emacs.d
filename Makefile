.FORCE:

emacs ?= emacs

BASEDIR := $(shell pwd)

profile:
	$(emacs) -Q -l lib/profile-dotemacs/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs

pull:
	git pull
	git submodule init
	git submodule update

upgrade: pull
	cd lib/org && make compile && make autoloads
	cd $(BASEDIR) && $(emacs) -batch -l packages.el

up: upgrade
	$(emacs) -Q -l init.el

.PHONY: profile pull upgrade up
