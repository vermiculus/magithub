-include config.mk

PACKAGE_BASENAME      := magithub
EMAKE_SHA1            := 1b23379eb5a9f82d3e2d227d0f217864e40f23e0

# override defaults
ifeq ($(MELPA_STABLE),true)
PACKAGE_ARCHIVES      := gnu melpa-stable
else
PACKAGE_ARCHIVES      := gnu melpa
endif

PACKAGE_TEST_ARCHIVES := gnu melpa

include emake.mk

.DEFAULT_GOAL: help

### Magithub targets

.PHONY: clean install compile test

clean:
	rm -rf $(EMAKE_WORKDIR)
	rm -rf *.elc

emake.mk: ## download the emake Makefile
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.mk'

test: compile test-ert ## run tests
lint: lint-package-lint lint-checkdoc ## run lints


### Manual-building

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../dash
ORG_LOAD_PATH += -L ../org/lisp
ORG_LOAD_PATH += -L ../org/contrib/lisp
ORG_LOAD_PATH += -L ../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
EMACS            ?= emacs
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/the.css

doc: info html html-dir pdf ## generate most manual formats
info: $(PACKAGE_BASENAME).info dir ## generate info manual
html: $(PACKAGE_BASENAME).html ## generate html manual file
pdf:  $(PACKAGE_BASENAME).pdf ## generate pdf manual

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
texi:				## generate texi manual (see comments)
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

html-dir: $(PACKAGE_BASENAME).texi ## generate html manual directory
	@printf "Generating $(PACKAGE_BASENAME)/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) $<

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null
