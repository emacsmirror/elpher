include config.mk

.PHONY: all autoloads clean compile help info package

.SUFFIXES: .texi .info

all: clean package

autoloads: $(PKG)-loaddefs.el

compile: clean $(PKG).elc

help:
	$(info make help      - show brief help)
	$(info make           - generate a tar file containing the package)
	$(info make all       - ditto)
	$(info make package   - ditto)
	$(info make info      - generate the info manual)
	$(info make compile   - byte-compile the package lisp files)
	$(info make autoloads - create $(PKG)-loaddefs.el to load Elpher in-place)
	$(info make clean     - remove generated files)
	@exit

%.info: %.texi
	$(MAKEINFO) --no-split $< -o $@

%.html: %.texi
	$(MAKEINFO) --no-split --html $< -o $@

%.pdf: %.texi
	$(MAKEINFO) --no-split --pdf $< -o $@

dir: $(PKG).info
	$(INSTALLINFO) $? $@

info: $(PKG).info dir

$(PKG)-$(VERSION).tar: $(PKG).info dir *.el COPYING
	mkdir $(PKG)-$(VERSION)
	cp -a $? $(PKG)-$(VERSION)/
	$(TAR) -cf $@ $(PKG)-$(VERSION)
	rm -rf $(PKG)-$(VERSION)

package: $(PKG)-$(VERSION).tar

clean:
	rm -f $(PKG).elc $(PKG)-loaddefs.el
	rm -f $(PKG).info dir $(PKG)-$(VERSION).tar
	rm -f $(PKG).html $(PKG).pdf $(PKG).aux $(PKG).fn $(PKG).fns $(PKG).log $(PKG).toc

define LOADDEFS_TPL
(add-to-list 'load-path (directory-file-name\n\
........................(or (file-name-directory #$$) (car load-path))))
endef
#' (ends emacs font-face garbage due to previous single quote)

$(PKG)-loaddefs.el:
	$(EMACS) -L $(PWD) \
		--eval "(setq-default backup-inhibited t)" \
		--eval "(setq generated-autoload-file \"$(PWD)/$@\")" \
		--eval "(update-directory-autoloads \"$(PWD)\")"
	sed -i "s/^;;; Code:$$/;;; Code:\n\n$(subst ., ,$(LOADDEFS_TPL))/" $@

$(PKG).elc:
	$(EMACS) -f batch-byte-compile $(@:.elc=.el)
