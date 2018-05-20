-include config.mk

PKG = magithub

EMACS      ?= emacs
EMACS_ARGS ?=

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../dash
ORG_LOAD_PATH += -L ../org/lisp
ORG_LOAD_PATH += -L ../org/contrib/lisp
ORG_LOAD_PATH += -L ../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/the.css

doc: info html html-dir pdf

help:
	$(info make doc          - generate most manual formats)
	$(info make texi         - generate texi manual (see comments))
	$(info make info         - generate info manual)
	$(info make html         - generate html manual file)
	$(info make html-dir     - generate html manual directory)
	$(info make pdf          - generate pdf manual)
	@printf "\n"

.PHONY: clean install build test

clean:
	rm -f *.elc
	rm -rf .cask/

install: .cask/
.cask/:
	cask --verbose install

build: .cask/
	cask build 2>&1 | tee build.log

test: test-build test-ert

# make sure there were no compile errors/warnings
test-build: build
	! grep -oe '.*:\(Error\|Warning\):.*' build.log

# run ERT tests
test-ert:
	cask exec ert-runner

setup-CI:
	export PATH="$(HOME)/bin:$(PATH)"
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'
	make -f emacs-travis.mk install_emacs
	make -f emacs-travis.mk install_cask
	emacs --version
	cask exec emacs --version

info: $(PKG).info dir
html: $(PKG).html
pdf:  $(PKG).pdf

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH) -l ox-extra -l ox-texinfo+.el
ORG_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# This target first bumps version strings in the Org source.  The
# necessary tools might be missing so other targets do not depend
# on this target and it has to be run explicitly when appropriate.
#
#   AMEND=t make texi    Update manual to be amended to HEAD.
#   VERSION=N make texi  Update manual for release.
#
.PHONY: texi
texi:
	@$(EMACS) $(ORG_ARGS) $(PKG).org $(ORG_EVAL)
	@printf "\n" >> $(PKG).texi
	@rm -f $(PKG).texi~

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

html-dir: $(PKG).texi
	@printf "Generating $(PKG)/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) $<

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null
