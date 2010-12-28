EMACS=emacs
XEMACS=xemacs
ADDITIONAL_LOAD_PATH=
LISPDIR=/usr/local/share/emacs/site-lisp
XLISPDIR=/usr/local/lib/xemacs/site-lisp
SHELL = /bin/sh
MKDIR = mkdir -p

all: eval.el
	@if test -z "`echo $(EMACS)|grep -i xemacs`"; then \
	  $(EMACS) -batch -q -no-site-file -l eval.el \
	  -f batch-byte-compile x-face.el; \
	else \
	  $(EMACS) -batch -q -no-site-file -f batch-byte-compile \
	  x-face.el x-face-xmas.el; \
	fi

eval.el: eval.el.in
	@echo 'sed '\''s^_@ADDITIONAL_LOAD_PATH@^_'$(ADDITIONAL_LOAD_PATH)'^_'\'' < eval.el.in > eval.el'
	@sed 's@ADDITIONAL_LOAD_PATH@$(ADDITIONAL_LOAD_PATH)' < eval.el.in > eval.el

install:	all
	@test -d $(LISPDIR) || $(MKDIR) $(LISPDIR)
	@if test -z "`echo $(EMACS)|grep -i xemacs`"; then \
	  echo 'Copying x-face.el to '$(LISPDIR)'...'; \
	  cp -p x-face.el $(LISPDIR); \
	  echo 'Copying x-face.elc to '$(LISPDIR)'...'; \
	  cp -p x-face.elc $(LISPDIR); \
	  echo Done; \
	else \
	  echo 'Copying x-face.el,  x-face-xmas.el  to '$(LISPDIR)'...'; \
	  cp -p x-face.el x-face-xmas.el $(LISPDIR); \
	  echo 'Copying x-face.elc, x-face-xmas.elc to '$(LISPDIR)'...'; \
	  cp -p x-face.elc x-face-xmas.elc $(LISPDIR); \
	  echo Done; \
	fi

lpath:
	@$(EMACS) -batch -q -no-site-file -eval \
	  '(mapcar \
	    (function (lambda (path) (princ (format "%s\n" path)))) \
	    load-path)'

x:
	@$(XEMACS) -batch -q -no-site-file -f batch-byte-compile \
	x-face.el x-face-xmas.el

xinstall:	x
	@test -d $(XLISPDIR) || $(MKDIR) $(XLISPDIR)
	@echo 'Copying x-face.el,  x-face-xmas.el  to '$(XLISPDIR)'...'
	@cp -p *.el $(XLISPDIR)
	@echo 'Copying x-face.elc, x-face-xmas.elc to '$(XLISPDIR)'...'
	@cp -p *.elc $(XLISPDIR)
	@echo Done

xlpath:
	@$(XEMACS) -batch -q -no-site-file -eval \
	  '(mapcar \
	    (function (lambda (path) (princ (format "%s\n" path)))) \
	    load-path)'

clean:
	rm -f eval.el *.elc *~
