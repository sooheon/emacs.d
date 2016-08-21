emacs ?= emacs

BASEDIR := $(shell pwd)

profile:
	$(emacs) -Q -l lib/profile-dotemacs/profile-dotemacs.el \
	--eval "(progn (setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath init.el)\")) \
	(set-background-color \"#F9F9F9\"))" \
	-f profile-dotemacs

install: upgrade
	cd lib/org && make compile && make autoloads
	cd $(BASEDIR) make run

pull:
	git pull
	git submodule init
	git submodule update

upgrade: pull
	cd $(BASEDIR) && $(emacs) -batch -l lisp/packages.el

up: upgrade
	run

run:
	$(emacs) -Q -l init.el

.PHONY: profile install pull upgrade up run
