emacs ?= emacs

BASEDIR := $(shell pwd)

profile:
	$(emacs) -debug-init -Q -l lib/profile-dotemacs/profile-dotemacs.el \
	--eval "(progn (setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath init.el)\")) \
	(set-background-color \"#F9F9F9\"))" \
	-f profile-dotemacs

pull:
	git pull
	git submodule init
	git submodule update

upgrade: pull
	cd $(BASEDIR) && $(emacs) -batch -l lisp/packages.el

run:
	$(emacs) -Q -l init.el

up: upgrade
	run

install: upgrade
	cd lib/org-mode && make compile && make autoloads
	cd $(BASEDIR) make run

mininal:
	$(emacs) -Q -l lisp/minimal-init.el

.PHONY: profile install pull upgrade up run
