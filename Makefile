-include config.mk

PACKAGE_BASENAME = magithub

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

EENVS  = PACKAGE_FILE="magithub.el"
EENVS += PACKAGE_TESTS="test/test-helper.el test/magithub-test.el"
EENVS += PACKAGE_LISP="$(wildcard magithub*.el)"

ifeq ($(MELPA_STABLE),true)
EENVS += PACKAGE_ARCHIVES="gnu melpa-stable"
else
EENVS += PACKAGE_ARCHIVES="gnu melpa"
endif

EMAKE := $(EENVS) emacs -batch -l emake.el --eval "(emake (pop argv))"

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
	rm -rf .elpa/

emake.el:
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'

.elpa/: emake.el
	$(EMAKE) install

install: .elpa/

build: install
	$(EMAKE) compile ~error-on-warn

test: build test-ert

# run ERT tests
test-ert: emake.el
	$(EMAKE) test ert

emacs-travis.mk:
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'
setup-CI: emacs-travis.mk
	export PATH="$(HOME)/bin:$(PATH)"
	make -f emacs-travis.mk install_emacs
	emacs --version

info: $(PACKAGE_BASENAME).info dir
html: $(PACKAGE_BASENAME).html
pdf:  $(PACKAGE_BASENAME).pdf

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
	@$(EMACS) $(ORG_ARGS) $(PACKAGE_BASENAME).org $(ORG_EVAL)
	@printf "\n" >> $(PACKAGE_BASENAME).texi
	@rm -f $(PACKAGE_BASENAME).texi~

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PACKAGE_BASENAME).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

html-dir: $(PACKAGE_BASENAME).texi
	@printf "Generating $(PACKAGE_BASENAME)/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) $<

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null
