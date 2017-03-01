;;; x-face.el -- X-Face encoder/decoder/viewer for Emacsen.

;; Copyright (C) 1996-2003 Katsumi Yamaoka
;; Copyright (C) 1996-2003 Tatsuya Ichikawa
;; Copyright (C) 1996-2003 OKUNISHI Fujikazu
;; Copyright (C) 1996-2003 KOSEKI Yoshinori
;; Copyright (C) 1996-2003 Ken'ichi OKADA
;; Copyright (C) 1996-2003 Tetsuya Uemura
;; Copyright (C) 1996-2003 TAKAHASHI Kaoru
;; Copyright (C) 2002-2003 Daiki Ueno
;; Copyright (C) 2003      Tatsuya Kinoshita
;; Author: Katsumi Yamaoka   <yamaoka@jpl.org>
;;         Tatsuya Ichikawa  <ichikawa@jpl.org>
;;         OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;         KOSEKI Yoshinori  <kose@yk.netlaputa.or.jp>
;;         Ken'ichi OKADA    <okada@opaopa.org>
;;         Tetsuya Uemura    <uemura@pp.iij4u.or.jp>
;;         TAKAHASHI Kaoru   <kaoru@kaisei.org>
;;         Daiki Ueno        <ueno@unixuser.org>
;;         Tatsuya Kinoshita <tats@vega.ocn.ne.jp>
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 1996/09/19
;; Revised: 2003/08/26
;; Keywords: x-face, x-face-mule, semi-gnus, gnus, mew, mail, mh-e,
;;           rmail, vm, cmail, wl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Now the X-Face utility for Emacs 21 is available at:
;;
;; ftp://ftp.jpl.org/pub/elisp/x-face-e21.el.gz
;;
;; We strongly recommend you use it rather than this module if you are
;; using Emacs 21 having a new redisplay engine.

;; This module provides the following five commands.  Each command name
;; expresses its function literally:
;;
;; x-face-encode
;;  Read an XBM file, encode it to the X-Face data, and show it.
;; x-face-insert
;;  Read an XBM file, encode it to the X-Face data, and insert it to
;;  the current buffer as a message header.
;; x-face-save
;;  Extract X-Face headers in the current buffer, encode them to XBM
;;  data, and save them in the local system.
;; x-face-view
;;  Extract X-Face headers in the current buffer, encode them to XBM
;;  data, and show them using an external image viewer.
;; x-face-ascii-view
;;  Extract X-Face headers in the current buffer, encode them to XBM
;;  data, and show them as ascii pictures in an Emacs frame."
;;
;; Please note, this module provides no image viewer in an Emacs frame
;; for the historical reason.  However, it is easy to display X-Face
;; images inline; Gnus provides the X-Face viewer by itself, you can
;; use x-face-mule.el for Mule 2 and Emacs, x-face-e21.el for Emacs 21,
;; x-face-xmas.el for XEmacs, and so forth.

;;; Code:

(defconst x-face-version-number "1.3.6.24"
  "Version number for this version of x-face.")
(defconst x-face-version
  (format "X-Face utility v%s" x-face-version-number)
  "Version string for this version of x-face.")
(defconst x-face-codename
;;  "Across The Universe"
;;  "Blackbird"
;;  "Carry That Weight"
;;  "Dr. Robert"
;;  "Eleanor Rigby"
;;  "Fixing A Hole"
;;  "Good Day Sunshine"
;;  "Honey Pie"
;;  "I Feel Fine"
;;  "Julia"
;;  "Kansas City"
;;  "Lovely Rita"
;;  "Martha My Dear"
;;  "Norwegian Wood"
;;  "Ob-la-di,Ob-la-da"
;;  "Penny Lane"
;;  "Rocky Raccoon"
;;  "Strawberry Fields Forever"
;;  "Ticket To Ride"
;;  "Back In The U.S.S.R"
;;  "She's LeaVing Home"
;;  "We Can Work It Out"
;;  "TaXman"
;;  "Yellow Submarine"
;;  "Zexy Sedie"
;;  "And Your Bird Can Sing"
;;  "Baby's In Back"
;;  "Can't Buy Me Love"
;;  "Day Tripper"
;;  "Every Little Thing"
;;  "The Fool On The Hill"
;;  "Good Night"
;;  "Hello Goodbye"
;;  "I Me Mine"
;;  "Yellow Submarine (remix)"
;;  "Hey Bulldog (remix)"
;;  "Eleanor Rigby (remix)"
;;  "Love You To (remix)"
  "All Together Now (remix)"
;;  "Lucy in the Sky With Diamonds (remix)"
;;  "Think for Yourself (remix)"
;;  "Sgt. Pepper's Lonely Hearts Club Band (remix)"
;;  "With a Little Help from My Friends (remix)"
;;  "Baby You're a Rich Man (remix)"
;;  "Only a Northern Song (remix)"
;;  "All You Need Is Love (remix)"
;;  "When I'm Sixty-Four (remix)"
;;  "Nowhere Man (remix)"
;;  "It's All Too Much (remix)"
  "Codename of this version of x-face (a string).")

;; For NEmacs.
(or (fboundp 'eval-and-compile)
    (progn
      (put 'eval-and-compile 'lisp-indent-hook 0)
      (defmacro eval-and-compile (&rest body)
	(cons 'progn body))))
(or (fboundp 'eval-when-compile)
    (progn
      (put 'eval-when-compile 'lisp-indent-hook 0)
      (defmacro eval-when-compile (&rest body)
	(cons 'progn body))))

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil)))

(eval-when-compile
  (let ((customizable (and (featurep 'custom)
			   (fboundp 'custom-declare-variable))))
    (unless customizable
      (defmacro defgroup (&rest args))
      (defmacro defcustom (symbol value &optional doc &rest args)
	(let ((doc (concat "*" (or doc ""))))
	  (` (defvar (, symbol) (, value) (, doc))))))
    ;; Silence the byte compiler.
    (save-excursion
      (beginning-of-defun)
      (eval-region (point-min) (point)))
    (let (case-fold-search)
      (if (string-match "(\\(alpha\\|beta\\)[0-9]+)$" x-face-version-number)
	  (mapcar
	   (function
	    (lambda (symbol)
	      (unless (boundp symbol)
		(make-local-variable symbol)
		(eval (list 'setq symbol nil)))))
	   (append
	    '(cmail-current-folder
	      custom-background-mode
	      font-lock-background-mode
	      frame-background-mode
	      frame-title-format
	      gnus-Article-buffer
	      gnus-article-buffer
	      gnus-background-mode
	      gnus-current-article
	      gnus-original-article-buffer
	      gnus-show-all-headers
	      gnus-show-mime
	      hilit-background-mode
	      mew-draft-buffer-header
	      mew-header-separator
	      mew-marker-header-end
	      mew-mv:
	      mh-folder-filename
	      mh-show-buffer
	      select-xface-version
	      vm-mail-buffer
	      x-face-gzip-compress-option
	      x-face-gzip-program
	      x-face-gzip-uncompress-option
	      x-face-mule-original-x-face-fields
	      x-face-mule:original-x-face-fields
	      x-face-mule-original-from-field
	      x-face-mule:original-from-field
	      x-face-use-gzip-when-save
	      x-face-xbm-file-directory
	      x-face-xmas-auto-image
	      x-face-xmas-image-field-cache
	      x-face-xmas-image-file-cache
	      x-face-xmas-last-highlight-headers-hack-x-face-p
	      x-face-xmas-xface-p)
	    (unless customizable
	      '(:data :prefix))))
	(make-local-variable 'byte-compile-warnings)
	(setq byte-compile-warnings nil)))))

(defgroup x-face nil
  "X-Face utility."
  :prefix "x-face-"
  :group 'applications)

(defcustom x-face-image-file-directory
  (or (and (boundp 'x-face-xbm-file-directory)
	   x-face-xbm-file-directory)
      "~/x-faces")
  "Directory name where image files will be found."
  :group 'x-face
  :type 'directory)

(defcustom x-face-image-file-directory-for-save
  x-face-image-file-directory
  "Directory name where image files will be saved in."
  :group 'x-face
  :type 'directory)

(defcustom x-face-default-xbm-file nil
  "Default xbm file name for user's face."
  :group 'x-face
  :type 'string)

(defcustom x-face-insert-interactive t
  "If non-nil, x-face-insert will be called interactively."
  :group 'x-face
  :type 'boolean)

(defcustom x-face-compressor
  (and (or (not (boundp 'x-face-use-gzip-when-save))
	   x-face-use-gzip-when-save)
       'gzip)
  "Compress method for saving image files.
This variable can be set to `nil' `gzip' or 'bzip2'."
  :group 'x-face
  :type '(radio (variable-item gzip)
		(variable-item bzip2)
		symbol))

(defcustom x-face-compressor-program-list
  (list
   (cons 'gzip
	 (if (and (boundp 'x-face-gzip-program)
		  (boundp 'x-face-gzip-compress-option)
		  (boundp 'x-face-gzip-uncompress-option))
	     (list ".gz"
		   x-face-gzip-program
		   x-face-gzip-compress-option
		   "GZIPping"
		   x-face-gzip-program
		   x-face-gzip-uncompress-option
		   "GUNZIPping")
	   '(".gz" "gzip" "-c9" "GZIPping" "gzip" "-cd" "GUNZIPping")))
   '(bzip2 ".bz2" "bzip2" "-f" "BZIPping" "bzip2" "-d" "BUNZIPping"))
  "List of compression methods.  Each element looks like (METHOD
FILENAME-SUFFIX COMPRESS-PROGRAM COMPRESS-ARGS COMPRESS-MSG
UNCOMPRESS-PROGRAM UNCOMPRESS-ARGS UNCOMPRESS-MSG)."
  :group 'x-face
  :type 'sexp)

(defcustom x-face-user-x-face-variable-name "x-face-encoded-string"
  "Variable name for the encoded X-Face string."
  :group 'x-face
  :type 'string)

(defcustom x-face-compface-program "compface"
  "Program used to encode X-Face."
  :group 'x-face
  :type 'string)

(defcustom x-face-uncompface-program "uncompface"
  "Program used to decode X-Face."
  :group 'x-face
  :type 'string)

(defcustom x-face-viewer-program
  (cond ((eq 'OS/2 system-type)		"pmview")
	((eq 'windows-nt system-type)	"start")
	(t				"xv"))
  "Program used to view images."
  :group 'x-face
  :type 'string)

(defcustom x-face-viewer-options
  (cond ((eq 'OS/2 system-type)		nil)
	((eq 'windows-nt system-type)	nil)
	(t				'("-nodecor" "-")))
  "Options used for a viewer program."
  :group 'x-face
  :type 'sexp)

(defcustom x-face-icons-to-color-picture-function
  (cond ((memq system-type '(OS/2 emx)) 'x-face-icons-to-os2-bmp)
	((eq 'windows-nt system-type)  'x-face-icons-to-win-bmp)
	(t 'x-face-icons-to-xpm))
  "Function used to make color pictures."
  :group 'x-face
  :type '(radio (function-item x-face-icons-to-os2-bmp)
		(function-item x-face-icons-to-win-bmp)
		(function-item x-face-icons-to-xpm)
		function))

(defcustom x-face-view-asynchronous (memq system-type '(OS/2 emx windows-nt))
  "Non-nil makes the external viewer run asynchronously."
  :group 'x-face
  :type 'boolean)

(defcustom x-face-view-asynchronous-function
  (cond ((memq system-type '(OS/2 emx))
	 'x-face-view-asynchronous-function-for-OS/2)
	((eq 'windows-nt system-type)
	 'x-face-view-asynchronous-function-for-win95)
	(t
	 'x-face-view-asynchronous-function-for-some))
  "Function used to show images asynchronously."
  :group 'x-face
  :type '(radio (function-item x-face-view-asynchronous-function-for-OS/2)
		(function-item x-face-view-asynchronous-function-for-win95)
		(function-item x-face-view-asynchronous-function-for-some)
		function))

(defcustom x-face-viewer-startup-time
  (cond ((eq 'OS/2 system-type)		2)
	((eq 'emx system-type)		2)
	(t				10))
  "Number of seconds to wait for the starting up the external viewer."
  :group 'x-face
  :type 'integer)

(defcustom x-face-save-or-view-method-alist
  '((gnus-summary-mode
     . (x-face-gnus-get-article
	x-face-find-gnus-article-buffer
	x-face-gnus-unwind-after-save-or-view))
    (gnus-Subject-mode
     . (x-face-gnus-get-article
	x-face-find-gnus-article-buffer
	x-face-gnus-unwind-after-save-or-view))
    (mh-folder-mode
     . (x-face-mh-get-article
	x-face-extracted-headers-buffer))
    (mew-summary-mode
     . (x-face-mew-get-article
	x-face-extracted-headers-buffer))
    (mew-virtual-mode
     . (x-face-mew-get-article
	x-face-extracted-headers-buffer))
    (rmail-mode
     . (x-face-rmail-get-all-headers
	x-face-extracted-headers-buffer))
    (vm-summary-mode
     . (x-face-vm-get-all-headers
	x-face-extracted-headers-buffer))
    (cmail-summary-mode
     . (x-face-cmail-get-article
	x-face-extracted-headers-buffer))
    (wl-summary-mode
     . (x-face-wl-get-article
	x-face-extracted-headers-buffer)))
  "Alist for saving or viewing X-Face images.  Each element should be
specified as follows:

'(MAJOR-MODE . (PREPARE-FUNCTION BUFFERorFUNCTION POST-FUNCTION))"
  :group 'x-face
  :type 'sexp)

(defcustom x-face-load-hook nil
  "Hook run after the x-face package is loaded."
  :group 'x-face
  :type 'hook)

(defcustom x-face-insert-hook nil
  "Hook run after inserting X-Face headers."
  :group 'x-face
  :type 'hook)

(defcustom x-face-add-x-face-version-header nil
  "If non-nil, the X-Face-Version header will be added."
  :group 'x-face
  :type 'boolean)

(defcustom x-face-ascii-picture-table
  (if (or (and (boundp 'custom-background-mode)
	       (eq 'dark custom-background-mode))
	  (and (boundp 'font-lock-background-mode)
	       (eq 'dark font-lock-background-mode))
	  (and (boundp 'frame-background-mode)
	       (eq 'dark frame-background-mode))
	  (and (boundp 'gnus-background-mode)
	       (eq 'dark gnus-background-mode))
	  (and (boundp 'hilit-background-mode)
	       (eq 'dark hilit-background-mode)))
      ["$" "\"" "0" " "]
    [" " "o" "\"" "$"])
  "Bitmap table used to ascii picture conversion.
\[0 0 1 1]
\[0 1 0 1]"
  :group 'x-face
  :type '(radio (choice-item :tag "Dark" ["$" "\"" "0" " "])
		(choice-item :tag "Light" [" " "o" "\"" "$"])))

(defcustom x-face-create-directory-function
  (if (fboundp 'make-directory)
      'make-directory
    (function
     (lambda (dir)
       (call-process "/bin/mkdir" nil nil nil dir)
       (or (file-directory-p dir)
	   (error "!! Can't create directory %s." dir)))))
  "Function used to make a directory."
  :group 'x-face
  :type 'function)

(defcustom x-face-tmp-directory
  (file-name-as-directory
   (cond ((fboundp 'temp-directory)
	  (temp-directory))
	 ((boundp 'temporary-file-directory)
	  temporary-file-directory)
	 ((memq system-type '(ms-dos windows-nt))
	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	 ((memq system-type '(vax-vms axp-vms))
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "SYS$SCRATCH:"))
	 (t
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
  "Name of the temporary directory."
  :group 'x-face
  :type 'directory)

(defcustom x-face-tmp-directory-name-format
  (let ((r 0))
    (while (<= r 0) (setq r (random)))
    (format ".x-face-%s-%d-%%d" (user-login-name) r))
  "Format used for temporary subdirectory names.
Subdirectories will be created and deleted in the temporary directory
specified by the `x-face-tmp-directory' variable."
  :group 'x-face
  :type 'string)

(defcustom x-face-inhibit-loadup-splash nil
  "If non-nil, the loadup splash will not be displayed."
  :group 'x-face
  :type 'boolean)

(defcustom x-face-debug nil nil
  :group 'x-face
  :type 'boolean)


;;; Internal variables.
(defconst x-face-mirror-table
  [?0 ?8 ?4 ?C ?2 ?A ?6 ?E ?1 ?9 ?5 ?D ?3 ?B ?7 ?F]
  "Mirror conversion table.")

(defconst x-face-mirror-table-binary
  [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15]
  "Mirror conversion table in binary.")

(defconst x-face-mirror-table-8bit-binary
  (eval-when-compile
    (let ((table (make-vector 256 nil))
	  (i 0)
	  j k l)
      (while (< i 256)
	(setq j i
	      k 0
	      l 8)
	(while (> l 0)
	  (setq k (lsh k -1)
		l (1- l))
	  (if (>= (setq j (lsh j 1)) 256)
	      (setq j (logand 255 j)
		    k (logior 128 k))))
	(aset table i k)
	(setq i (1+ i)))
      table))
  "Mirror conversion table in 8-bit binary.")

(defvar x-face-extracted-headers-buffer nil)

(defvar x-face-tmp-file-number 1)
(defvar x-face-post-messages-save nil)

(defconst x-face-nop (function (lambda (&rest args))))

(defvar x-face-last-message nil)

(make-variable-buffer-local (defconst x-face-completions nil))

(defvar x-face-delete-file-alist)
(setq x-face-delete-file-alist nil)


;;; Anti XEmacs.
(unless (featurep 'xemacs)
  (mapcar (function (lambda (symbol)
		      (eval (list 'defvar symbol))
		      (eval (list 'setq symbol nil))))
	  '(x-face-xmas-auto-image
	    x-face-xmas-image-field-cache
	    x-face-xmas-image-file-cache
	    x-face-xmas-last-highlight-headers-hack-x-face-p
	    x-face-xmas-xface-p)))


;;; Emulations for Emacs variants.
(defconst x-face-no-conversion
  (cond (;; XEmacs
	 (featurep 'xemacs) (coding-system-name (get-coding-system nil)))
	(;; Mule 2.3
	 (boundp 'MULE) '*noconv*)
	(;; Emacs 20.x, MULE 3.0
	 (fboundp 'check-coding-system)
	 (or (condition-case ()
		 (check-coding-system 'binary)
	       (coding-system-error nil))
	     (condition-case ()
		 (check-coding-system 'no-conversion)
	       (coding-system-error nil))))))

(mapcar
 (function
  (lambda (elt)
    (let ((func (car elt)))
      (or (fboundp func)
	  (fset func (symbol-function (cdr elt)))))))
 '((buffer-substring-no-properties . buffer-substring)
   (defsubst . defun)
   (read-char-exclusive . read-char)))

(or (fboundp 'buffer-disable-undo)
    (defun buffer-disable-undo (&optional buffer)
      "Make BUFFER stop keeping undo information."
      (buffer-flush-undo (or buffer (current-buffer)))))

(or (fboundp 'match-string)
    (defun match-string (num &optional string)
      "Return string of text matched by last search.
\(This function is stolen from XEmacs for NEmacs.)"
      (if (match-beginning num)
	  (if string
	      (substring string (match-beginning num) (match-end num))
	    (buffer-substring (match-beginning num) (match-end num))))))

(or (fboundp 'redisplay-frame)
    (defun redisplay-frame (&rest args)
      (sit-for 0)))

(eval-and-compile
  (or (condition-case ()
	  (require 'alist)	;; APEL
	(error nil))
      (condition-case ()
	  (require 'tl-list)	;; tm
	(error nil))
      (fboundp 'put-alist)
      ;; Imported from alist.el
      (defun put-alist (item value alist)
	"Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST."
	(let ((pair (assoc item alist)))
	  (if pair
	      (progn
		(setcdr pair value)
		alist)
	    (cons (cons item value) alist))))))

(or (fboundp 'delete-file)
    (defun delete-file (filename)
      (call-process "/bin/rm" nil nil nil "-f" filename)))


;;; Macros and inline functions.
(defmacro x-face-following-char ()
  (if (fboundp 'char-after)
      '(char-after (point))
    '(following-char)))

(defmacro x-face-read-file-name (prompt dir default mustmatch initial)
  "Read file name interactively."
  (let ((major (string-to-int emacs-version))
	(minor (and (boundp 'emacs-minor-version) emacs-minor-version)))
    (cond ((> 19 major)
	   (list 'read-file-name prompt
		 (list 'concat dir initial) default mustmatch))
	  ((and (eq 19 major) (or (not minor) (> 29 minor)))
	   ;; 19.28's read-file-name() is evil.
	   (list
	    'let
	    (list (list 'dir (list 'if (list 'string-match "/$" dir)
				   dir
				   (list 'concat dir "/"))))
	    (list 'completing-read
		  prompt
		  (list 'mapcar
			'(function (lambda (name) (list (concat dir name))))
			(list 'delete ".."
			      (list 'delete "."
				    (list 'directory-files
					  (list 'or 'dir
						'default-directory)))))
		  nil mustmatch (list 'concat 'dir initial))))
	  (t
	   (list 'read-file-name prompt dir default mustmatch initial)))))

(defsubst x-face-buffer-live-p (buffer)
  "Say whether BUFFER is alive or not."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer))))

(defsubst x-face-delcr ()
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n"))
  (goto-char (point-min)))

(defsubst x-face-set-buffer-disable-undo (buffer)
  (set-buffer buffer)
  (buffer-disable-undo))

(defmacro x-face-header-separator-regexp ()
  '(concat
    "^"
    (mapconcat
     (quote identity)
     (delq nil
	   (list (and (boundp (quote mail-header-separator))
		      (stringp mail-header-separator)
		      (regexp-quote mail-header-separator))
		 (and (boundp (quote mew-header-separator))
		      (stringp mew-header-separator)
		      (regexp-quote mew-header-separator))
		 "-.*-"
		 ""))
     "$\\|^")
    "$"))

(defmacro x-face-hex-char-to-binary (hex)
  ;; (+ (logand hex 15) (* (lsh hex -6) 9)))
  (list '+ (list 'logand hex 15) (list '* (list 'lsh hex -6) 9)))

(defmacro x-face-generate-file-name (real-dir name num spec compr)
  (list 'let
	(list
	 (list 'base
	       (list 'if num
		     (list 'format "%s-%02d" name (list 'incf num))
		     (list 'setq num 0)
		     name)))
	(list 'list num
	      (list 'concat real-dir "/" 'base "." spec compr)
	      'base)))

(defmacro x-face-file-name-as-directory (name &optional noslash)
  (list 'concat
	(list 'if (list 'string-match "/+$" name)
	      (list 'substring name 0 (list 'match-beginning 0))
	      name)
	(list 'if noslash "" "/")))

(defmacro x-face-abbreviate-file-name (filename)
  (if (fboundp 'abbreviate-file-name)
      (if (featurep 'xemacs)
	  (list 'abbreviate-file-name filename t)
	(list 'abbreviate-file-name filename))
    (list 'if (list 'string-match
		    '(concat "^" (regexp-quote (expand-file-name "~/")))
		    filename)
	  (list 'concat "~/" (list 'substring filename '(match-end 0)))
	  filename)))

(defsubst x-face-detect-x-face-type ()
  (let ((case-fold-search t)
	beg end)
    (goto-char (point-min))
    (and (search-forward "/*" nil t)
	 (setq beg (point))
	 (search-forward "*/" nil t)
	 (setq end (match-beginning 0))
	 (goto-char beg)
	 (cond ((re-search-forward "type[\t \"]*[:;=]+[\t \"]*rgb" end t)
		'rgb)
	       ((re-search-forward "type[\t \"]*[:;=]+[\t \"]*mono" end t)
		'mono)))))

(defmacro x-face-filename-suffix ()
  '(or (cadr (assq x-face-compressor x-face-compressor-program-list)) ""))

(defmacro x-face-compressed-filename-regexp (form)
  (list 'format form
	'(mapconcat
	  (function (lambda (elt) (regexp-quote (cadr elt))))
	  x-face-compressor-program-list "\\|")))

(defmacro x-face-filename-suffix-to-copmressor (suffix)
  (list 'let '((list x-face-compressor-program-list) compr)
	(list 'while 'list
	      (list 'if (list 'string-equal suffix '(cadar list))
		    '(setq compr (caar list)
			   list nil)
		    '(setq list (cdr list))))
	'compr))

(defmacro x-face-directory-files
  (dirname &optional full match nosort files-only)
  (if (featurep 'xemacs)
      (list 'directory-files dirname full match nosort files-only)
    (list 'let
	  (list
	   (list 'files
		 (list 'delq
		       nil
		       (list 'mapcar
			     (list
			      'if full
			      (list 'if files-only
				    '(function
				      (lambda (file)
					(and (not
					      (file-directory-p
					       (expand-file-name file)))
					     file)))
				    '(quote identity))
			      (list 'if files-only
				    (list
				     'function
				     (list 'lambda '(file)
					   (list
					    'and
					    (list 'not
						  (list 'file-directory-p
							(list
							 'expand-file-name
							 'file dirname)))
					    'file)))
				    '(quote identity)))
			     (if (> (string-to-int emacs-version) 18)
				 (list 'directory-files
				       dirname full match nosort)
			       (list 'directory-files
				     dirname full match))))))
	  (if (<= (string-to-int emacs-version) 18)
	      (list 'if nosort
		    'files
		    '(require (quote sort))
		    '(sort files (quote string-lessp)))
	    'files))))


;;; Internal functions for general purpose.
(defun x-face-error (&rest args)
  "(x-face-error FORMAT ARG ARG ... BUFFER)."
  (let* ((len (length args))
	 (buffer (nth (1- len) args)))
    (cond ((zerop len) (error ""))
	  ((and (> len 1) (x-face-buffer-live-p buffer))
	   (and x-face-debug
		(let ((errbuf (generate-new-buffer
			       (concat "*x-face-error-("
				       (buffer-name buffer)
				       ")*")))
		      content point)
		  (save-excursion
		    (set-buffer buffer)
		    (setq content (buffer-substring-no-properties
				   (point-min)
				   (point-max))
			  point (point))
		    (x-face-set-buffer-disable-undo errbuf)
		    (insert content)
		    (goto-char point)
		    (set-buffer-modified-p nil)
		    (setq buffer-read-only t)
		    (cond ((fboundp 'set-buffer-file-coding-system)
			   (set-buffer-file-coding-system
			    x-face-no-conversion))
			  ((fboundp 'set-file-coding-system)
			   (set-file-coding-system x-face-no-conversion))
			  ((fboundp 'set-kanji-fileio-code)
			   (set-kanji-fileio-code nil))))
		  (display-buffer errbuf)))
	   (apply 'error (reverse (cdr (reverse args)))))
	  (t (apply 'error args)))))

(defun x-face-expand-file-name (filename)
  "Convert FILENAME to absolute pathname."
  (and (stringp filename)
       (let ((name (file-name-nondirectory filename))
	     (directory (expand-file-name (file-name-directory filename)))
	     (regexp (x-face-compressed-filename-regexp "\\(%s\\)$")))
	 (and
	  (string-match regexp name)
	  (setq name (substring name 0 (match-beginning 0))))
	 (or (car (x-face-directory-files
		   directory t (concat "^" (regexp-quote name) regexp) nil t))
	     (and (file-exists-p (setq filename (concat directory name)))
		  filename)))))

(defun x-face-read-existing-file-name (&optional pre-prompt non-interactive)
  "Read existing xbm file name."
  (let* ((dir1 (and (stringp x-face-default-xbm-file)
		    (file-name-directory x-face-default-xbm-file)))
	 (dir (or (and dir1
		       (x-face-expand-file-name x-face-default-xbm-file)
		       (x-face-abbreviate-file-name dir1))
		  (and (stringp x-face-image-file-directory)
		       (x-face-file-name-as-directory
			x-face-image-file-directory))))
	 (initial (and dir
		       (stringp x-face-default-xbm-file)
		       (if (and dir1
				(x-face-expand-file-name
				 x-face-default-xbm-file))
			   (file-name-nondirectory x-face-default-xbm-file)
			 (and (x-face-expand-file-name
			       (concat dir x-face-default-xbm-file))
			      x-face-default-xbm-file))))
	 (file (if non-interactive
		   (and dir initial (concat dir initial))
		 (x-face-read-file-name
		  (concat (or pre-prompt "") "xbm file: ")
		  (or dir default-directory) nil t initial))))
    (and (setq file (x-face-expand-file-name file))
	 (not (file-directory-p file))
	 file)))

(defun x-face-save-buffer-no-conversion (filename &optional silent)
  (let (;; NEmacs
	kanji-flag
	;; Mule
	(output-coding-system x-face-no-conversion)
	;; Mule, XEmacs
	write-region-pre-hook
	;; XEmacs (<= 20.1b6)
	(file-coding-system x-face-no-conversion)
	;; XEmacs (>= 20.1b7)
	(coding-system-for-write x-face-no-conversion)
	;; jam-zcat, jka-compr
	jam-zcat-filename-list
	jka-compr-compression-info-list)
    (write-region (point-min) (point-max) filename nil silent)))

(defun x-face-call-process-buffer-no-conversion (program &rest program-args)
  (let (;; NEmacs
	(default-kanji-process-code 0)
	program-kanji-code-alist
	;; Mule
	(default-process-coding-system
	  (cons x-face-no-conversion x-face-no-conversion))
	call-process-hook
	;; XEmacs (<= 20.1b6)
	(file-coding-system x-face-no-conversion)
	;; XEmacs (>= 20.1b7)
	(buffer-file-coding-system x-face-no-conversion)
	;; XEmacs (>= 20.2b1)
	(coding-system-for-read x-face-no-conversion)
	(coding-system-for-write x-face-no-conversion))
    (apply 'call-process-region (point-min) (point-max)
	   program t (current-buffer) nil program-args)))

(defun x-face-compress-buffer ()
  (let ((elt (assq x-face-compressor x-face-compressor-program-list))
	prog arg msg)
    (and elt
	 (setq prog (nth 2 elt))
	 (setq arg (nth 3 elt))
	 (setq msg (nth 4 elt))
	 (progn
	   (message "%s..." msg)
	   (x-face-call-process-buffer-no-conversion prog arg)
	   (message "")))))

(defun x-face-uncompress-buffer (compr)
  (let ((elt (assq compr x-face-compressor-program-list))
	prog arg msg)
    (and elt
	 (setq prog (nth 5 elt))
	 (setq arg (nth 6 elt))
	 (setq msg (nth 7 elt))
	 (progn
	   (or x-face-last-message (message "%s..." msg))
	   (x-face-call-process-buffer-no-conversion prog arg)
	   (or x-face-last-message (message ""))))))

(defun x-face-find-file-no-conversion (filename &rest args)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let (;; NEmacs
	kanji-flag
	;; Mule
	(input-coding-system x-face-no-conversion)
	;; XEmacs (<= 20.1b5)
	(overriding-file-coding-system x-face-no-conversion)
	;; XEmacs (>= 20.1b6)
	(coding-system-for-read x-face-no-conversion)
	;; Mule,XEmacs
	buffer-file-format
	format-alist
	insert-file-contents-post-hook
	insert-file-contents-pre-hook
	;; jam-zcat, jka-compr
	jam-zcat-filename-list
	jka-compr-compression-info-list)
    (apply 'insert-file-contents filename args))
  (cond ((string-match (x-face-compressed-filename-regexp "\\(%s\\)$")
		       filename)
	 (x-face-uncompress-buffer
	  (x-face-filename-suffix-to-copmressor
	   (match-string 1 filename))))
	((looking-at "\037\213")
	 (x-face-uncompress-buffer 'gzip)))
  (x-face-delcr))

(defun x-face-narrow-to-header ()
  "Narrow to the header of the message."
  (goto-char (point-min))
  (narrow-to-region
   (point)
   (or (and (re-search-forward (x-face-header-separator-regexp) nil t)
	    (match-beginning 0))
       (point-max)))
  (goto-char (point-min)))

(defun x-face-search-field (regexp &optional case-fold)
  "Re-search for specified field and returns some points."
  (let ((case-fold-search case-fold))
    (and (re-search-forward (concat regexp ":[\t\n ]*") nil t)
	 (list (match-beginning 0)
	       (match-end 0)
	       (goto-char (or (and (re-search-forward "^[^\n\t ]\\|^$" nil t)
				   (match-beginning 0))
			      (point-max)))))))

(defun x-face-mail-fetch-field (field)
  "Return the value of the header field FIELD."
  (or (let ((case-fold-search t)
	    points)
	(and (setq points (x-face-search-field
			   (concat "^" (regexp-quote field)) t))
	     (buffer-substring-no-properties
	      (nth 1 points) (nth 2 points))))
      ""))

(defun x-face-extract-mail-address (str)
  "Extract mail address from a string STR."
  (cond ((string-match "^[\n\t ,]*(.*)[\n\t ,]*" str) "nobody")
	((string-match "<\\([^\n\t ,]+\\)>" str)
	 (substring str (match-beginning 1) (match-end 1)))
	((string-match "[^\n\t (),<>]+" str)
	 (substring str (match-beginning 0) (match-end 0)))
	(t "nobody")))

(defun x-face-mail-fetch-mail-address ()
  "Extract a mail address from the header."
  (let (end str buffer)
    (save-excursion
      (goto-char (point-min))
      (setq end (and (re-search-forward "^$" nil t)
		     (match-beginning 0)))
      (goto-char (point-min))
      (while (re-search-forward "^From:" end t)
	(setq str
	      (append
	       str
	       (list (buffer-substring-no-properties
		      (match-beginning 0)
		      (or (and (re-search-forward "^[^\n\t ]" end t)
			       (match-beginning 0))
			  (goto-char end)))))))
      (when str
	(setq buffer (get-buffer-create " *x-face-from*"))
	(set-buffer buffer)
	(setq buffer-read-only nil)
	(erase-buffer)
	(mapcar 'insert str)
	(goto-char (point-min))
	(while (re-search-forward "[^\C-a-\C-?]+" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "^From:[\t ]*\n" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(prog1
	    (x-face-extract-mail-address (x-face-mail-fetch-field "from"))
	  (kill-buffer buffer))))))

(defun x-face-Y-or-n-p (prompt)
  (if (and (featurep 'xemacs) (should-use-dialog-box-p))
      (yes-or-no-p-dialog-box prompt)
    (let ((cursor-in-echo-area t)
	  (echo-keystrokes 0)
	  (log-message-max-size 0)
	  (inhibit-quit t)
	  message-log-max ans)
      (while (not (memq ans '(?\  ?N ?Y ?\C-g ?\e ?\n ?\r ?n ?y)))
	(message "%s(Y/n) " prompt)
	(setq ans (read-char-exclusive)))
      (not (memq ans '(?\C-g ?N ?n))))))

(defun x-face-messages (msgs)
  (if msgs
      (let ((buffer (get-buffer-create "*x-face-message*"))
	    (window-min-height 2)
	    (width (window-width))
	    height
	    (mini (minibuffer-window))
	    (echo-keystrokes 0)
	    start)
	(save-excursion
	  (x-face-set-buffer-disable-undo buffer)
	  (setq buffer-read-only nil)
	  (goto-char (point-max))
	  (insert "-- " (current-time-string) " --\n")
	  (setq start (point))
	  (narrow-to-region start start)
	  (or (listp msgs) (setq msgs (list msgs)))
	  (mapcar
	   (function
	    (lambda (msg)
	      (insert (if (stringp msg)
			  msg
			(prin1-to-string msg)))
	      (or (bolp) (insert "\n"))))
	   msgs)
	  (goto-char (point-min))
	  (while (re-search-forward "^[\r\t ]+" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "\n\n+" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (while (progn
		     (end-of-line)
		     (not (< (current-column) width)))
	      (move-to-column (1- width))
	      (insert "\n "))
	    (forward-line 1))
	  (setq height (count-lines (point-min) (point-max)))
	  (when (eq 1 height)
	    (message (buffer-substring-no-properties
		      (point-min) (1- (point-max)))))
	  (widen))
	(unless (eq 1 height)
	  (message "")
	  (while (not (eq (next-window nil t) mini))
	    (other-window -1))
	  (unless (eq buffer (current-buffer))
	    (split-window-vertically)
	    (other-window 1)
	    (switch-to-buffer buffer))
	  (shrink-window (- (window-height) (+ 3 height)))
	  (goto-char (point-max))
	  (insert "\n Press any key >")
	  (set-buffer-modified-p nil)
	  (set-window-start (get-buffer-window buffer) start)
	  (or (prog1
		  (eq ?\C-g (let ((inhibit-quit t)) (read-char-exclusive)))
		(forward-line -1)
		(delete-region (point) (point-max))
		(set-buffer-modified-p nil)
		(delete-window)))))
    (message "")))

(defun x-face-minibuffer-completion-help ()
  (remove-hook 'minibuffer-setup-hook 'x-face-minibuffer-completion-help)
  (make-local-variable 'frame-title-format)
  (setq frame-title-format
	(concat "*X-Face " (cdr (assq this-command
				      '((x-face-menu-encode . "Encode")
					(x-face-menu-insert . "Insert"))))
		"*"))
  (minibuffer-completion-help))


;;; Internal functions for encoding.
(defun x-face-split-icons (width height)
  (if (and (eq 48 height) (eq 48 width))
      (list (current-buffer))
    (let ((ycount 0)
	  xcount
	  (yscale (/ height 48))
	  (xscale (/ width 48))
	  (name (buffer-name (current-buffer)))
	  buffer buffers start lines
	  (cur (current-buffer)))
      (while (< ycount yscale)
	(setq xcount 0)
	(while (< xcount xscale)
	  (setq buffer (generate-new-buffer name)
		buffers (append buffers (list buffer))
		start (+ 1 (* xcount 12) (* ycount width 12))
		lines 48)
	  (save-excursion
	    (x-face-set-buffer-disable-undo buffer)
	    (while (> lines 0)
	      (insert-buffer-substring cur start (+ start 12))
	      (setq start (+ start (/ width 4)))
	      (decf lines))
	    (set-buffer-modified-p nil)
	    (incf xcount)))
	(incf ycount))
      (kill-buffer cur)
      buffers)))

(defun x-face-xbm-to-icon-buffers ()
  "Convert xbm to icon format and return the list of spec and buffers."
  (let ((case-fold-search t)
	(type (x-face-detect-x-face-type))
	width height xbytes right margin)

    (goto-char (point-min))
    (or (re-search-forward "_width[\t ]+\\([0-9]+\\)" nil t)
	(x-face-error "!! Illegal xbm file format." (current-buffer)))
    (setq width (string-to-int (match-string 1))
	  xbytes (/ (+ width 7) 8))
    (goto-char (point-min))
    (or (re-search-forward "_height[\t ]+\\([0-9]+\\)" nil t)
	(x-face-error "!! Illegal xbm file format." (current-buffer)))
    (setq height (string-to-int (match-string 1)))

    (goto-char (point-min))
    (re-search-forward "0x[0-9a-f][0-9a-f],")
    (delete-region (point-min) (match-beginning 0))

    (goto-char (point-min))
    (while (re-search-forward "[\n\r\t ,;}]\\|0x" nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (not (eobp))
      (setq right (aref
		   x-face-mirror-table
		   (x-face-hex-char-to-binary (x-face-following-char))))
      (delete-char 1)
      (insert-char (aref
		    x-face-mirror-table
		    (x-face-hex-char-to-binary (x-face-following-char)))
		   1)
      (delete-char 1)
      (insert-char right 1))

    (unless (zerop (setq margin (% (- 6 (% xbytes 6)) 6)))
      (goto-char (point-min))
      (while (not (eobp))
	(forward-char (* 2 xbytes))
	(insert-char ?0 (* 2 margin)))
      (setq xbytes (+ xbytes margin)
	    width (* 8 xbytes)))

    (unless (zerop (setq margin (% (- 48 (% height 48)) 48)))
      (goto-char (point-max))
      (insert-char ?0 (* 2 xbytes margin))
      (setq height (+ height margin)))

    (set-buffer-modified-p nil)
    (append
     (list (if (zerop (% (* width height) 6912))		;; UNKNOWN: nil
	       type					;; RGB:    'rgb
	     'mono))					;; MONO:   'mono
     (list (cons (/ width 48) (/ height 48)))		;; geometry (x . y)
     (x-face-split-icons width height)			;; icon buffers
     )))

(defun x-face-xbm-to-icon-strings ()
  "Convert xbm to icon format and return the list of spec and strings."
  (let ((data (x-face-xbm-to-icon-buffers)))
    (list (pop data) (pop data)
	  (mapcar
	   (function
	    (lambda (buffer)
	      (prog1
		  (save-excursion
		    (set-buffer buffer)
		    (buffer-string))
		(kill-buffer buffer))))
	   data))))

(defun x-face-icon-buffer-to-x-face-buffer ()
  "Convert icon buffer to x-face string in the buffer."
  (while (re-search-forward "/\\*\\(.\\|\n\\)*\\*/[\n\t ]*" nil t)
    (replace-match ""))
  (x-face-call-process-buffer-no-conversion x-face-compface-program)
  (x-face-delcr)
  (looking-at "[\t ][^\t ]+$"))

(defun x-face-icon-strings-to-x-face-strings (icons)
  "Convert icon strings to x-face strings."
  (let ((buffer (get-buffer-create " *x-face*")))
    (prog1
	(save-excursion
	  (set-buffer buffer)
	  (setq buffer-read-only nil)
	  (mapcar
	   (function
	    (lambda (icon)
	      (erase-buffer)
	      (insert icon)
	      (x-face-call-process-buffer-no-conversion
	       x-face-compface-program)
	      (x-face-delcr)
	      (buffer-string)))
	   icons))
      (kill-buffer buffer))))

(defun x-face-insert-version-header ()
  (let ((version
	 (apply 'concat
		(append
		 (list x-face-version)
		 (if (zerop (length x-face-codename))
		     '("")
		   (list " - \"" x-face-codename "\""))
		 (or (and (not (eq 'select-xface-out this-command))
			  (boundp 'select-xface-version)
			  (stringp select-xface-version)
			  (list "\n                with "
				select-xface-version))
		     '(""))))))
    (if (bobp)
	(insert "X-Face-Version: " version "\n")
      (backward-char 1)
      (insert "\nX-Face-Version: " version)
      (forward-char 1))))


;;; Internal functions for decoding.
(defun x-face-x-face-encoded-string-to-icon-string (string)
  "Convert X-Face encoded string to icon string."
  (let ((buffer (get-buffer-create " *x-face*")))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert string)
      (x-face-call-process-buffer-no-conversion x-face-uncompface-program)
      (goto-char (point-min))
      (while (re-search-forward "[\r\n,]\\|0x" nil t)
	(replace-match "")
	(goto-char (point-min)))
      (setq string (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer buffer)
    string))

(defun x-face-x-face-region-to-icon-string (beg end)
  "Convert X-Face field to icon string."
  (x-face-x-face-encoded-string-to-icon-string
   (buffer-substring-no-properties beg end)))

(defun x-face-extract-x-face-fields-to-icons (&optional single glyph)
  "Extract X-Face fields to icons.  It returns the list of
\(FIELD_or_nil TYPE X Y DATA DATA ...) or (FIELD_or_nil GLYPH)."
  (let ((case-fold-search t)
	buffer-undo-list
	(modified-p (buffer-modified-p))
	buffer-read-only
	(inhibit-read-only t)
	(re1 (if single
		 "^X-Face:[\n\t ]*"
	       "^\\(X-Face-Type\\|X-Face\\(-[0-9]+\\)?\\):[\n\t ]*"))
	(re2 (concat "geometry[\t \"'`]*=[\t \"'`]*"
		     "\\([0-9]+\\)[\t \"'`]*x[\t \"'`]*\\([0-9]+\\)"))
	(re3 (concat "animate[\t \"'`]*=[\t \"'`]*"
		     "\\([\.0-9]+\\)[\t \"'`]*"))
	(re4 (concat "depth[\t \"'`]*=[\t \"'`]*"
		     "\\([0-9]+\\)[\t \"'`]*"))
	beg end prop filename cache attributes modtime size image
	rgb gray depth geometry num field fields faces x-faces animate)
    (set-buffer-modified-p nil)
    (save-restriction
      (x-face-narrow-to-header)
      (while (re-search-forward re1 nil t)
	(setq beg (point))
	(beginning-of-line)
	;; Insert a fake X-Face-Type field.
	(unless (or single (looking-at "X-Face-Type"))
	  (save-excursion
	    (setq depth 0)
	    (save-excursion
	      (while (re-search-forward "^X-Face\\(-[0-9]+\\)?:" nil t)
		(and (match-beginning 1) (null gray)
		     (setq gray t))
		(incf depth)))
	    (when gray
	      (re-search-forward "^X-Face\\(-[0-9]+\\)?:" nil t)
	      (goto-char (match-beginning 0))
	      (insert "X-Face-Type: ")
	      (setq beg (point))
	      (insert (format "GRAY; depth=%d; geometry=1x1\n" depth))
	      (set-buffer-modified-p t))))
	(setq
	 x-faces
	 (append
	  x-faces
	  (if (prog1
		  (and (not single) (looking-at "X-Face-Type"))
		(goto-char beg)
		(setq end (or (and (re-search-forward "^[^\n\t ]" nil t)
				   (goto-char (match-beginning 0)))
			      (point-max))
		      prop (and (featurep 'xemacs)
				(get-text-property
				 (1- end) 'x-face-file-attributes))
		      filename (car prop)
		      cache (and (featurep 'xemacs)
				 (assoc filename
					x-face-xmas-image-file-cache)))
		(and glyph
		     cache
		     (setq attributes (file-attributes filename)
			   modtime (nth 5 attributes)
			   size (nth 7 attributes))
		     (equal modtime (cadr cache))
		     (eq size (caddr cache))
		     (setq image (cadddr cache))))
	      (progn
		(goto-char beg)
		(setq rgb (or (looking-at "[\t \"'`]*RGB")
			      (re-search-forward ";[\t \"'`]*RGB" end t)))
		(goto-char beg)
		(setq gray (or (looking-at "[\t \"'`]*GRAY")
			      (re-search-forward ";[\t \"'`]*GRAY" end t)))
		(goto-char beg)
		(setq depth (and (re-search-forward re4 end t)
				 (string-to-number (match-string 1))))
		(goto-char beg)
		(setq animate (and (re-search-forward re3 end t)
				   (string-to-number (match-string 1))))
		(setq animate (and (numberp animate) (max animate 0.05)))
		(goto-char beg)
		(setq geometry (and (re-search-forward re2 end t)
				    (list (string-to-int (match-string 1))
					  (string-to-int (match-string 2))))
		      num (apply '* (if rgb 3 (if gray depth 1)) geometry))
		(or (and image
			 (prog1
			     (list (if (or rgb (eq 1 (length image)))
				       (cdar image)
				     (cddr image)))
			   (goto-char end)
			   (while (and (> num 0) (not (eobp)))
			     (and (re-search-forward "^X-Face\\(-[0-9]+\\)?:[\n\t ]*" nil t)
				  (goto-char
				   (or
				    (and (re-search-forward "^[^\n\t ]" nil t)
					 (match-beginning 0))
				    (point-max)))
				  (decf num)))))
		    (progn
		      (setq field (format "X-Face-Type: %s%s%s%s%s\n"
					  (if rgb
					      "RGB"
					    (if gray
						"GRAY"
					      ""))
					  (if (or geometry animate depth)
					      "; "
					    "")
					  (if depth
					      (if (or geometry animate)
						  (format "depth=%g; "
							  depth)
						(format "depth=%g" depth))
					    "")
					  (if animate
					      (if geometry
						  (format "animate=%g; "
							  animate)
						(format "animate=%g" animate)
						(setq geometry '(1 1)))
					    "")
					  (if geometry
					      (apply 'format
						     "geometry=%dx%d" geometry)
					    "")))
		      (if (and (or rgb gray) (null geometry))
			  (setq geometry '(1 1)))
		      (goto-char (setq beg end))
		      (setq end
			    (or (and (re-search-forward "^X-Face-Type:" nil t)
				     (match-beginning 0))
				(point-max)))
		      (goto-char beg)
		      (setq faces nil)
		      (while (and (or (> num 0) animate)
				  (re-search-forward "X-Face\\(-[0-9]+\\)?:[\n\t ]*" end t))
			(decf num)
			(setq faces
			      (append
			       faces
			       (list (buffer-substring-no-properties
				      (point)
				      (or (and (re-search-forward
						"^[^\n\t ]" end t)
					       (goto-char (match-beginning 0)))
					  end))))))
		      (setq faces (mapcar
				   (function
				    (lambda (string)
				      (while (string-match "[\n\t ]+" string)
					(setq string
					      (concat
					       (substring
						string 0 (match-beginning 0))
					       (substring string
							  (match-end 0)))))
				      string))
				   faces)
			    field (concat field "X-Face: "
					  (mapconcat 'identity faces
						     "\nX-Face: ")
					  "\n")
			    fields (append fields (list field)))
		      (or (and glyph
			       (cdr
				(assoc field x-face-xmas-image-field-cache)))
			  (progn
			    (setq faces
				  (mapcar
				   'x-face-x-face-encoded-string-to-icon-string
				   faces))
			    (if (and geometry
				     (or animate
					 (eq (apply '* (if rgb 3 (if gray depth 1)) geometry)
					     (length faces))))
				(list
				 (append (cons (if rgb 'rgb (if gray 'gray 'mono)) geometry)
					 (list animate) faces))
			      (list
			       (append (list 'mono (length faces) 1 animate)
				       faces))))))))
	    (list
	     (if (and x-face-xmas-xface-p glyph)
		 (prog1
		     (setq glyph
			   (make-glyph
			    (vector
			     'xface :data
			     (concat
			      "X-Face: "
			      (buffer-substring-no-properties beg end)))))
		   (set-glyph-face glyph 'x-face-xmas-x-face-face))
	       (list 'mono 1 1 nil
		     (x-face-x-face-region-to-icon-string beg end)))))))))
    ;; Remove fake X-Face-Type fields.
    (when (buffer-modified-p)
      (push nil buffer-undo-list)
      (let* ((fname (if (featurep 'xemacs)
			'display-message
		      'message))
	     (fdef (symbol-function fname)))
	;; Mute "Undo!" message.
	(defalias fname 'ignore)
	(unwind-protect
	    (condition-case nil
		(undo)
	      (error))
	  (defalias fname fdef))))
    (set-buffer-modified-p modified-p)
    (mapcar
     (function (lambda (elt) (cons (pop fields) elt)))
     x-faces)))

(defun x-face-extract-x-face-fields-to-single-icons-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (mapcar
     (function (lambda (elt) (car (cdr (cdr (cdr (cdr (cdr elt))))))))
     (x-face-extract-x-face-fields-to-icons 'single))))

(defun x-face-concat-mono-icons (x y icons)
  "Concatenate multiple monochrome icons."
  (if (eq 1 x)
      (apply 'concat icons)
    (let (i j k m)
      (apply
       'concat
       (mapcar
	(function (lambda (n)
		    (setq i ""
			  j 0)
		    (while (< j 48)
		      (setq i (apply
			       'concat
			       i
			       (mapcar
				(function (lambda (icon)
					    (setq k (* 12 j))
					    (substring icon k (+ 12 k))))
				n)))
		      (incf j))
		    i))
	(mapcar
	 (function (lambda (o)
		     (setq m nil)
		     (while (> o 0)
		       (push (pop icons) m)
		       (decf o))
		     (nreverse m)))
	 (make-list y x)))))))

(defun x-face-concat-rgb-icons (x y icons)
  "Concatenate multiple RGB icons."
  (let ((xy (* x y))
	p)
    (mapcar
     (function (lambda (q) (x-face-concat-mono-icons x y q)))
     (mapcar
      (function (lambda (r)
		  (setq p nil)
		  (while (> r 0)
		    (push (pop icons) p)
		    (decf r))
		  (nreverse p)))
      (make-list 3 xy)))))

(defun x-face-ascii-to-binary-icon (str &optional x upsidedown xmas)
  (let ((p 0)
	(max (length str))
	q c bin)
    (when upsidedown
      (setq upsidedown ""
	    x (* 12 x))
      (while (< p max)
	(setq upsidedown
	      (concat (substring str p (setq p (+ p x))) upsidedown)))
      (setq str upsidedown
	    p 0))
    (while (< p max)
      (setq q (1+ p)
	    c (x-face-hex-char-to-binary (string-to-char (substring str p q)))
	    p q)
      (and xmas
	   (setq q (1+ p)
		 c (+ (* 16 (aref x-face-mirror-table-binary
				  (x-face-hex-char-to-binary
				   (string-to-char (substring str p q)))))
		      (aref x-face-mirror-table-binary c))
		 p q))
      (push c bin))
    (nreverse bin)))

(defun x-face-icons-to-xpm (name x y icons &optional spec)
  "Generate an XPM color picture from three icons."
  (if spec
      "xpm"
    (save-excursion
      (let ((buffer (set-buffer (generate-new-buffer " *xpm*")))
	    (reds (x-face-ascii-to-binary-icon (pop icons)))
	    (grns (x-face-ascii-to-binary-icon (pop icons)))
	    (blus (x-face-ascii-to-binary-icon (pop icons)))
	    (table [" " "c" "m" "b" "y" "g" "r" "#"])
	    (width (* 48 x))
	    (height (* 48 y))
	    start red grn blu)
	(insert "/* XPM */\n"
		"static char *" name "[] = {\n"
		"/* width height ncolors chars_per_pixel */\n"
		(format "\"%d %d 8 1\",\n" width height)
		"/* colors */\n"
		"\"  c #FFFFFF\",\n"
		"\"c c #00FFFF\",\n"
		"\"m c #FF00FF\",\n"
		"\"b c #0000FF\",\n"
		"\"y c #FFFF00\",\n"
		"\"g c #00FF00\",\n"
		"\"r c #FF0000\",\n"
		"\"# c #000000\",\n"
		"/* pixels */\n")
	(setq start (point))
	(while (setq red (pop reds)
		     grn (pop grns)
		     blu (pop blus))
	  (insert (aref table (logior (lsh (logand 8 red) -3)
				      (lsh (logand 8 grn) -2)
				      (lsh (logand 8 blu) -1)))
		  (aref table (logior (lsh (logand 4 red) -2)
				      (lsh (logand 4 grn) -1)
				      (logand 4 blu)))
		  (aref table (logior (lsh (logand 2 red) -1)
				      (logand 2 grn)
				      (lsh (logand 2 blu) 1)))
		  (aref table (logior (logand 1 red)
				      (lsh (logand 1 grn) 1)
				      (lsh (logand 1 blu) 2)))))
	(goto-char start)
	(while (not (eobp))
	  (insert "\"")
	  (forward-char width)
	  (insert "\",\n"))
	(backward-delete-char 2)
	(insert "\n};\n")
	buffer))))

(defun x-face-icons-to-bmp-subr (x y icons header)
  (save-excursion
    (let ((buffer (set-buffer (generate-new-buffer " *bmp*")))
	  (reds (x-face-ascii-to-binary-icon (pop icons) x t))
	  (grns (x-face-ascii-to-binary-icon (pop icons) x t))
	  (blus (x-face-ascii-to-binary-icon (pop icons) x t))
	  red grn blu)
      (mapcar
       (function
	(lambda (elt)
	  (if (consp elt)
	      (insert-char (car elt) (cdr elt))
	    (insert-char elt 1))))
       header)
      (while (setq red (pop reds)
		   grn (pop grns)
		   blu (pop blus))
	(insert-char (+ (* 16 (logior (lsh (logand 8 red) -3)
				      (lsh (logand 8 grn) -2)
				      (lsh (logand 8 blu) -1)))
			(logior (lsh (logand 4 red) -2)
				(lsh (logand 4 grn) -1)
				(logand 4 blu)))
		     1)
	(insert-char (+ (* 16 (logior (lsh (logand 2 red) -1)
				      (logand 2 grn)
				      (lsh (logand 2 blu) 1)))
			(logior (logand 1 red)
				(lsh (logand 1 grn) 1)
				(lsh (logand 1 blu) 2)))
		     1))
      buffer)))

(defun x-face-icons-to-os2-bmp (name x y icons &optional spec)
  "Generate an OS/2 BMP color picture from three icons."
  (if spec
      "bmp"
    (let ((size (+ 74 (* 1152 x y)))
	  (width (* 48 x))
	  (height (* 48 y)))
      (x-face-icons-to-bmp-subr
       x y icons
       (list
	;; BITMAPFILEHEADER
	66 77					;; bfType = "BM"
	(% size 256) (/ size 256) '(0 . 2)	;; bfSize
	'(0 . 4)				;; bfReserved1,bfReserved2
	74 '(0 . 3)				;; bfOffBits
	;; BITMAPINFOHEADER
	12 '(0 . 3)				;; ???
	(% width 256) (/ width 256)		;; biWidth
	(% height 256) (/ height 256)		;; biHeight
	1 0		;; biPlanes
	4 0		;; biBitCount
	;; RGBQUAD
	'(255 . 3)	;; White
	'(255 . 2) 0	;; Cyan
	255 0 255	;; Magenta
	255 '(0 . 2)	;; Blue
	0 '(255 . 2)	;; Yellow
	0 255 0		;; Green
	'(0 . 2) 255	;; Red
	'(0 . 3)	;; Black
	;; ???
	'(0 . 24)	;; ???
	)))))

(defun x-face-icons-to-win-bmp (name x y icons &optional spec)
  "Generate a Windoze BMP color picture from three icons."
  (if spec
      "bmp"
    (let ((size (+ 88 (* 1152 x y)))
	  (width (* 48 x))
	  (height (* 48 y)))
      (x-face-icons-to-bmp-subr
       x y icons
       (list
	;; BITMAPFILEHEADER
	66 77					;; bfType = "BM"
	(% size 256) (/ size 256) '(0 . 2)	;; bfSize
	'(0 . 4)				;; bfReserved1,bfReserved2
	86 '(0 . 3)				;; bfOffBits
	;; BITMAPINFOHEADER
	40 '(0 . 3)				;; biSize
	(% width 256) (/ width 256) '(0 . 2)	;; biWidth
	(% height 256) (/ height 256) '(0 . 2)	;; biHeight
	1 0		;; biPlanes
	4 0		;; biBitCount
	'(0 . 4)	;; biCompression
	'(0 . 4)	;; biSizeImage
	18 11 '(0 . 2)	;; biXPelsPerMeter
	18 11 '(0 . 2)	;; biYPelsPerMeter
	8 '(0 . 3)	;; biClrUsed
	8 '(0 . 3)	;; biClrImportant
	;; RGBQUAD
	'(255 . 3) 0	;; White
	'(255 . 2) '(0 . 2);; Cyan
	255 0 255 0	;; Magenta
	255 '(0 . 3)	;; Blue
	0 '(255 . 2) 0	;; Yellow
	0 255 '(0 . 2)	;; Green
	'(0 . 2) 255 0	;; Red
	'(0 . 4)	;; Black
	)))))

(defun x-face-icons-to-xbm (name x y icon)
  (save-excursion
    (let ((buffer (generate-new-buffer " *xbm*"))
	  w n)
      (set-buffer buffer)
      (insert icon)
      (goto-char (point-min))
      (while (not (eobp))
	(setq w (aref x-face-mirror-table
		      (x-face-hex-char-to-binary (x-face-following-char))))
	(delete-char 1)
	(insert-char
	 (aref x-face-mirror-table
	       (x-face-hex-char-to-binary (x-face-following-char)))
	 1)
	(delete-char 1)
	(insert-char w 1))
      (goto-char (point-min))
      (while (not (eobp))
	(insert " 0x")
	(setq n 14)
	(while (> n 0)
	  (decf n)
	  (forward-char 2)
	  (if (eobp)
	      (setq n 0)
	    (insert ",0x")))
	(or (eobp) (forward-char 2))
	(insert ",\n"))
      (backward-delete-char 2)
      (insert "};\n")
      (goto-char (point-min))
      (insert (format "#define %s_width %d\n" name (* 48 x))
	      (format "#define %s_height %d\n"  name (* 48 y))
	      (format "static char %s_bits[] = {\n" name))
      buffer)))

(defun x-face-xbm-to-pbm (xbm &optional raw mirrored)
  "Return a PBM image converted from an XBM image.  If the optional RAW
is non-nil, the raw PBM format will be used.  If the optional MIRRORED
is non-nil, the return value will be right and left mirrored for each
byte."
  (let (width height bits i rest)
    (with-temp-buffer
      (insert xbm)
      (goto-char (point-min))
      (setq case-fold-search t)
      (unless (and (search-forward "_width" nil t)
		   (numberp (setq width (read (current-buffer))))
		   (progn
		     (goto-char (point-min))
		     (search-forward "_height" nil t))
		   (numberp (setq height (read (current-buffer))))
		   (search-forward "{" nil t)
		   (progn
		     (delete-region (point-min) (1+ (point)))
		     t)
		   (search-forward "}" nil t)
		   (progn
		     (delete-region (1- (point)) (point-max))
		     (goto-char (point-min))
		     (while (search-forward "0x" nil t)
		       (replace-match "\\\\x"))
		     (goto-char (point-min))
		     (while (re-search-forward "[^0-9A-FX\\]+" nil t)
		       (replace-match ""))
		     (= (buffer-size) (/ (* width height) 2))))
	(erase-buffer)
	(insert-char 0 288)
	(setq width 48
	      height 48))
      (if mirrored
	  (setq bits (read (concat "\"" (buffer-string) "\"")))
	(dolist (byte (append (read (concat "\"" (buffer-string) "\"")) nil))
	  (push (char-to-string (aref x-face-mirror-table-8bit-binary byte))
		bits))
	(setq bits (apply 'concat (nreverse bits)))))
    (if raw
	(format "P4\n%d %d\n%s" width height bits)
      (dolist (byte (prog1
			(append bits nil)
		      (setq bits nil)))
	(setq i 8
	      rest nil)
	(while (> i 0)
	  (push (if (zerop (logand byte 1))
		    "0"
		  "1")
		rest)
	  (setq byte (lsh byte -1)
		i (1- i)))
	(setq bits (nconc bits rest)))
      (format "P1\n%d %d\n%s\n" width height
	      (mapconcat 'identity bits " ")))))

(defun x-face-gray-faces-to-xpm (faces)
  "Return an XPM image converted from plain PBM images.  Note that it
currently limits the depth to 6-bit, lower bits will be ignored."
  (dolist (face (prog1
		    (nthcdr (max 0 (- (length faces) 6)) (nreverse faces))
		  (setq faces nil)))
    (push (read (concat "[" (substring face 9) "]")) faces))
  (let* ((ix 0)
	 iy pixel
	 (depth (length faces))
	 (picture (make-vector 2304 nil))
	 (ncolors (expt 2 depth))
	 (step (/ 256 (1- ncolors)))
	 brt)
    (while (< ix 2304)
      (setq iy 0
	    pixel 0)
      (while (< iy depth)
	(setq pixel (+ pixel pixel (aref (nth iy faces) ix))
	      iy (1+ iy)))
      (aset picture ix pixel)
      (setq ix (1+ ix)))
    ;; Here's a raw PGM image.
    ;; (format "P5\n48 48\n%d\n%s" (1- ncolors) (concat picture))
    (with-temp-buffer
      (insert "\
/* XPM */
static char * Gray_X_Face[] = {
/* width height ncolors chars_per_pixel */
\"48 48 " (number-to-string ncolors) " 1\",
/* colors */
")
      (setq ix 0)
      (while (< ix ncolors)
	(setq iy (* ix step))
	(insert (format "\"%c c #%02x%02x%02x\",\n" (+ ix 35) iy iy iy))
	(setq ix (1+ ix)))
      (insert "\
/* pixels */
\"")
      (setq ix 0
	    iy 48)
      (while (< ix 2304)
	(insert (+ (aref picture ix) 35))
	(setq ix (1+ ix))
	(when (zerop (setq iy (1- iy)))
	  (insert "\",\n\"")
	  (setq iy 48)))
      (delete-backward-char 3)
      (insert "\n};\n")
      (buffer-string))))

(defun x-face-extract-x-face-fields-to-pictures (buffer name)
  "Extract X-Face fields and return pictures."
  (save-excursion
    (set-buffer buffer)
    (let ((data (x-face-extract-x-face-fields-to-icons))
	  (spec (funcall x-face-icons-to-color-picture-function
			 nil nil nil nil 'spec))
	  icons type x y pictures)
      (while (setq icons (cdr (pop data)))
	(setq type (pop icons)
	      x (pop icons)
	      y (pop icons))
	(pop icons)
	(push (if (eq 'rgb type)
		  (cons spec
			(funcall x-face-icons-to-color-picture-function
				 name x y (x-face-concat-rgb-icons x y icons)))
		(if (eq 'gray type)
		    (let ((buffer (get-buffer-create " *gray x-face*")))
		      (save-excursion
			(set-buffer buffer)
			(erase-buffer)
			(insert
			 (x-face-gray-faces-to-xpm
			  (mapcar
			   (lambda (icon)
			     (save-excursion
			       (set-buffer (x-face-icons-to-xbm
					    "noname" x y
					    icon))
			       (unwind-protect
				   (x-face-xbm-to-pbm (buffer-string))
				 (kill-buffer (current-buffer)))))
			   icons)))
			(cons "xpm" buffer)))
		  (cons "xbm"
			(x-face-icons-to-xbm
			 name x y (x-face-concat-mono-icons x y icons)))))
	      pictures))
      (nreverse pictures))))


;;; Internal functions for viewing and saving.
(defun x-face-view-buffer (spec_buffer)
  (let ((spec (car spec_buffer))
	(buffer (cdr spec_buffer))
	tmp-dir umask tmp-fname)
    (save-excursion
      (set-buffer buffer)
      (if (and x-face-view-asynchronous x-face-view-asynchronous-function)
	  (progn
	    (while (file-exists-p
		    (setq tmp-dir (expand-file-name
				   (format x-face-tmp-directory-name-format
					   x-face-tmp-file-number)
				   x-face-tmp-directory)))
	      (incf x-face-tmp-file-number))
	    (if (fboundp 'set-default-file-modes)
		(progn
		  (setq umask (default-file-modes))
		  (unwind-protect
		      (progn
			(set-default-file-modes 448) ;; decimal for octal 0700
			;; Create temporary directory.  If it has already been
			;; created by another user, an error will occur.
			(funcall x-face-create-directory-function tmp-dir))
		    (set-default-file-modes umask)))
	      (funcall x-face-create-directory-function tmp-dir)
	      (set-file-modes tmp-dir 448))
	    ;; tmp-dir has been created
	    (setq tmp-fname (expand-file-name (format "X-Face.%s" spec)
					      tmp-dir))
	    ;; tmp-fname is safe
	    (x-face-save-buffer-no-conversion tmp-fname 'silent)
	    (funcall x-face-view-asynchronous-function tmp-fname))
	(apply 'x-face-call-process-buffer-no-conversion
	       x-face-viewer-program x-face-viewer-options)))
    (kill-buffer buffer)))

(defun x-face-save-files (buffer name)
  "Save X-Face headers to files."
  (let ((directory (x-face-file-name-as-directory
		    x-face-image-file-directory-for-save 'noslash))
	(re (format "^%s\\(-[0-9]+\\)?\\.\\(bmp\\|xbm\\|xpm\\)\\(%s\\)?$"
		    (regexp-quote name)
		    (mapconcat
		     (function (lambda (elt) (regexp-quote (cadr elt))))
		     x-face-compressor-program-list "\\|")))
	(spec (funcall x-face-icons-to-color-picture-function
		       nil nil nil nil 'spec))
	(compr (x-face-filename-suffix))
	(case-fold-search t)
	real-dir exists num names fname
	data icons type x y)
    (setq real-dir (expand-file-name directory))
    (if (file-exists-p real-dir)
	(or (file-directory-p real-dir)
	    (error "!! %s is not a directory." directory))
      (funcall x-face-create-directory-function real-dir))
    (setq exists (x-face-directory-files real-dir nil re nil t)
	  num (and exists
		   (car (sort (mapcar
			       (function
				(lambda (str)
				  (if (and (string-match re str)
					   (setq str (match-string 1 str)))
				      (- (string-to-int str))
				    0)))
			       exists) '>))))
    (save-excursion
      (set-buffer buffer)
      (setq data (x-face-extract-x-face-fields-to-icons))
      (while (setq icons (cdr (pop data)))
	(setq type (pop icons)
	      x (pop icons)
	      y (pop icons))
	(pop icons)
	(if (eq 'rgb type)
	    (setq names (x-face-generate-file-name
			 real-dir name num spec compr)
		  num (pop names)
		  fname (pop names)
		  buffer (funcall x-face-icons-to-color-picture-function
				  (pop names) x y
				  (x-face-concat-rgb-icons x y icons)))
	  (setq names (x-face-generate-file-name real-dir name num "xbm" compr)
		num (pop names)
		fname (pop names)
		buffer (x-face-icons-to-xbm
			(pop names) x y (x-face-concat-mono-icons x y icons))))
	(save-excursion
	  (set-buffer buffer)
	  (x-face-compress-buffer)
	  (x-face-save-buffer-no-conversion fname 'silent))
	(setq x-face-post-messages-save
	      (append x-face-post-messages-save
		      (list (concat "Wrote "
				    (x-face-abbreviate-file-name fname)))))
	(kill-buffer buffer)))))

(defun x-face-icon-to-ascii-buffer (&optional keep-trailing-white-space)
  "Convert icon data to an ascii picture."
  (goto-char (point-min))
  (while (re-search-forward "/\\*\\(.\\|\n\\)*\\*/[\n\t ]*" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (let (p c1 c2 x a)
    (while (not (eobp))
      (setq x 12
	    a "")
      (while (> x 0)
	(decf x)
	(setq p (point)
	      c1 (x-face-hex-char-to-binary (x-face-following-char))
	      c1 (logior (lsh (logand c1 8) 4)
			 (lsh (logand c1 4) 3)
			 (lsh (logand c1 2) 2)
			 (lsh (logand c1 1) 1)))
	(goto-char (+ (point) 12))
	(setq c2 (x-face-hex-char-to-binary (x-face-following-char))
	      c2 (logior c1 (logior (lsh (logand c2 8) 3)
				    (lsh (logand c2 4) 2)
				    (lsh (logand c2 2) 1)
				    (logand c2 1)))
	      a (concat a
			(mapconcat
			 (function
			  (lambda (ref)
			    (aref x-face-ascii-picture-table ref)))
			 (list (lsh c2 -6)
			       (lsh (logand c2 48) -4)
			       (lsh (logand c2 12) -2)
			       (logand c2 3)) "")))
	(goto-char (1+ p)))
      (delete-region (- (point) 12) (+ (point) 12))
      (insert (if (and (not keep-trailing-white-space)
		       (string-match " +$" a))
		  (substring a 0 (match-beginning 0))
		a)
	      "\n")))
  (goto-char (point-min))
  (set-buffer-modified-p nil))

(defun x-face-view-single-ascii (icon)
  (let ((buffer (get-buffer-create " *x-face-ascii*"))
	(wconf (current-window-configuration)))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert icon)
    (x-face-icon-to-ascii-buffer)
    (pop-to-buffer buffer)
    (delete-other-windows)
    (set-window-start
     (get-buffer-window buffer)
     (if (< (window-height) 24)
	 (progn
	   (goto-char (point-min))
	   (re-search-forward "^\\( *\n\\)+" nil t)
	   (point))
       (point-min)))
    (message "Press any key to continue")
    (let ((fn (if (featurep 'xemacs)
		  'next-command-event
		'read-event)))
      (funcall fn)
      (discard-input))
    (erase-buffer)
    (insert-char ?\n 12)
    (insert "                 Please wait...")
    (message "")
    (redisplay-frame)
    (set-window-configuration wconf)))

(defun x-face-save-or-view (task)
  "Save or view X-Face headers."
  (let ((method (assq major-mode x-face-save-or-view-method-alist))
	(msg (cond ((eq 'save task) "Sav")
		   ((memq task '(view ascii)) "View")))
	prepare buffer post name)
    (and method
	 (setq method (cdr method)
	       prepare (car method)
	       buffer (cadr method)
	       post (caddr method)))
    (unwind-protect
	(progn
	  (message "Extracting X-Face fields...")
	  (and prepare
	       (let ((gnus-show-all-headers
		      (and (boundp 'gnus-show-all-headers)
			   gnus-show-all-headers))
		     (gnus-show-mime
		      (and (boundp 'gnus-show-mime) gnus-show-mime)))
		 (or (boundp 'gnus-original-article-buffer)
		     (setq gnus-show-all-headers t
			   gnus-show-mime nil))
		 (if (eq 'x-face-vm-get-all-headers prepare)
		     (funcall prepare (format "Extracing X-Face fields..."))
		   (funcall prepare))))
	  (setq buffer (condition-case ()
			   (if (fboundp buffer)
			       (funcall buffer)
			     (symbol-value buffer))
			 (error nil)))
	  (or (x-face-buffer-live-p buffer)
	      (setq buffer (current-buffer)))
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (x-face-narrow-to-header)
	      (or (re-search-forward "^X-Face\\(-[0-9]+\\)?:" nil t)
		  (x-face-error "!! No X-Face fields" buffer))
	      (and (memq task '(view save))
		   (goto-char (point-min))
		   (setq name (x-face-extract-mail-address
			       (x-face-mail-fetch-field "from"))))))
	  (cond ((eq 'view task)
		 (mapcar 'x-face-view-buffer
			 (prog1
			     (x-face-extract-x-face-fields-to-pictures
			      buffer name)
			   (message "Viewing X-Face..."))))
		((eq 'save task)
		 (x-face-save-files buffer name))
		((eq 'ascii task)
		 (mapcar 'x-face-view-single-ascii
			 (x-face-extract-x-face-fields-to-single-icons-buffer
			  buffer)))))
      (and post (funcall post))
      (when (eq 'save task)
	(x-face-messages x-face-post-messages-save)
	(setq x-face-post-messages-save nil))
      (and (x-face-buffer-live-p x-face-extracted-headers-buffer)
	   (kill-buffer x-face-extracted-headers-buffer))
      (and (memq task '(view ascii)) (message "%sing X-Face...done" msg)))))

(defun x-face-read-image-format ()
  (let ((table '(("xpm" . x-face-icons-to-xpm)
		 ("os2-bmp" . x-face-icons-to-os2-bmp)
		 ("win-bmp" . x-face-icons-to-win-bmp)))
	(format ""))
    (while (string-equal "" format)
      (setq format
	    (completing-read
	     "Color image format: " table nil t
	     (car (rassq x-face-icons-to-color-picture-function table)))))
    (cdr (assoc format table))))


;;; Commands.

;;;###autoload
(defun x-face-encode (file &optional arg)
  "Read an XBM file FILE, encode it to the X-Face data, and show it.
If ARG is given, show the Emacs-Lisp form instead."
  (interactive (list (x-face-read-existing-file-name "[Encode] ")
		     current-prefix-arg))
  (setq file (expand-file-name file))
  (let ((buf1 (get-buffer-create " *x-face*"))
	buf2 icons type geometry width height num msg x-faces start next str
	attributes modtime size cache glyph)
    (save-excursion
      (set-buffer buf1)
      (x-face-find-file-no-conversion file)
      (message "Converting xbm to icon(s)...")
      (setq icons (x-face-xbm-to-icon-strings))
      (kill-buffer buf1)
      (setq type (pop icons)
	    geometry (pop icons)
	    width (car geometry)
	    height (cdr geometry)
	    num (and (or (> width 1) (> height 1)) 0)
	    icons (car icons)
	    msg (if num
		    "Converting icons to x-faces..."
		  "Converting icon to x-face..."))
      (message msg)
      (setq x-faces (x-face-icon-strings-to-x-face-strings icons))
      (when (interactive-p)
	(x-face-set-buffer-disable-undo
	 (setq buf2 (generate-new-buffer
		     (format "*X-Face: %s*" (file-name-nondirectory file)))))
	(mapcar
	 (function (lambda (x-face) (insert "X-Face:" x-face)))
	 x-faces)
	(goto-char (point-min))
	(if arg
	    (while (re-search-forward "^X-Face:" nil t)
	      (replace-match "")
	      (insert "(setq " x-face-user-x-face-variable-name
		      (if num
			  (format "-%02d" (incf num))
			"")
		      "\n      ")
	      (setq start (point)
		    next (or (and (re-search-forward "^[^\n\t ]" nil t)
				  (goto-char (match-beginning 0)))
			     (point-max))
		    str (prin1-to-string (buffer-substring start next)))
	      (delete-region start next)
	      (insert str ")\n"))
	  (and x-face-add-x-face-version-header
	       (x-face-insert-version-header))
	  (when num
	    (if (or (eq 'rgb type)
		    (and (not type)
			 (x-face-Y-or-n-p "Is this X-Face type RGB? ")
			 (setq type 'rgb)))
		(insert (apply 'format "X-Face-Type: RGB; geometry=%dx%d\n"
			       (if (eq 1 height)
				   '(1 1)
				 (list width (/ height 3)))))
	      (insert (format "X-Face-Type: geometry=%dx%d\n" width height))))
	  (goto-char (point-min))
	  (and x-face-xmas-xface-p
	       (not (eq 'tty (device-type)))
	       (x-face-xmas-highlight-headers-hack-x-face-p-p)
	       (progn
		 (setq attributes (file-attributes file)
		       modtime (nth 5 attributes)
		       size (nth 7 attributes)
		       cache (assoc file x-face-xmas-image-file-cache)
		       glyph (cadddr cache))
		 (if (and (equal modtime (cadr cache))
			  (eq size (caddr cache)))
		     (setq glyph
			   (if (or (eq 'rgb type) (eq 1 (length glyph)))
			       (cdar glyph)
			     (cddr glyph)))
		   (and (setq
			 glyph
			 (car (x-face-xmas-extract-x-face-fields-to-glyphs)))
			(or (> width 1) (> height 1))
			(put-alist file
				   (list modtime size
					 (list (cons (* 48 height) glyph)))
				   x-face-xmas-image-file-cache)))
		 (and glyph (set-extent-begin-glyph
			     (make-extent (point) (point)) glyph))
		 (insert "\n"))))
	(goto-char (point-max))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(display-buffer buf2)
	(set-window-start (get-buffer-window buf2) 1))
      (message (concat msg "done")))
    (list (cons 'type type)
	  (cons 'width width)
	  (cons 'height height)
	  (cons 'data x-faces))))

;;;###autoload
(defun x-face-menu-encode ()
  "Generate X-Face string(s) from xbm file using menu."
  (interactive)
  (and (> (string-to-int emacs-version) 18)
       (let ((minibuffer-setup-hook minibuffer-setup-hook)
	     x-face-default-xbm-file completions)
	 (add-hook 'minibuffer-setup-hook 'x-face-minibuffer-completion-help)
	 (call-interactively 'x-face-encode)
	 (and (setq completions (get-buffer "*Completions*"))
	      (setq completions (get-buffer-window completions))
	      (delete-window completions)))))

;;;###autoload
(defun x-face-insert (&optional file append)
  "Read an XBM file FILE, encode it to the X-Face data, and insert it to
the current buffer as a message header.  If FILE is omitted, use a
file specified by the `x-face-default-xbm-file' variable.  Although it
normally deletes existing X-Face headers before inserting, you can
insert multiple X-Face headers using the optional argument APPEND (it
can be specified by the `C-u' key for the interactive use)."
  (interactive (list (x-face-read-existing-file-name "[Insert] ")
		     current-prefix-arg))
  (or file
      (interactive-p)
      (setq file
	    (x-face-read-existing-file-name "[Insert] "
					    (not x-face-insert-interactive))))
  (and file
       (let ((buffer (get-buffer-create " *x-face*"))
	     icons type geometry width height mult msg x-faces
	     points pt1 pt2 cache modtime size rate)
	 (save-excursion
	   (set-buffer buffer)
	   (x-face-find-file-no-conversion file)
	   (setq icons (x-face-xbm-to-icon-strings)
		 type (pop icons)
		 geometry (pop icons)
		 width (car geometry)
		 height (cdr geometry)
		 mult (or (> width 1) (> height 1))
		 icons (car icons)
		 msg (if mult
			 "Converting icons to x-faces...%s"
		       "Converting icon to x-face...%s")))
	 (message msg "")
	 (setq x-faces (x-face-icon-strings-to-x-face-strings icons))
	 (and (featurep 'xemacs) (x-face-xmas-remove-x-face-glyph))
	 (save-excursion
	   (save-restriction
	     (x-face-narrow-to-header)
	     (while (setq points (x-face-search-field "^x-face-version" t))
	       (delete-region (nth 0 points) (nth 2 points)))
	     (goto-char (point-min))
	     (if (setq points (x-face-search-field "^x-face\\(-.+\\)?" t)
		       pt1 (nth 0 points))
		 (if append
		     (while (x-face-search-field "^x-face\\(-.+\\)?" t))
		   (delete-region (nth 0 points) (nth 2 points))
		   (while (setq points
				(x-face-search-field "^x-face\\(-.+\\)?" t))
		     (delete-region (nth 0 points) (nth 2 points))))
	       (goto-char (point-max))
	       ;; for Mew 1.93b45
	       (and (eq 'mew-draft-mode major-mode)
		    (boundp 'mew-mv:)
		    (progn
		      (forward-line -1)
		      (or (looking-at mew-mv:) (goto-char (point-max))))))
	     (or pt1 (setq pt1 (point)))
	     (setq pt2 (point))
	     (mapcar
	      (function
	       (lambda (x-face)
		 (insert "X-Face:" x-face)))
	      x-faces)
	     (when mult
	       (goto-char pt2)
	       (if (or (eq 'rgb type)
		       (and (not type)
			    (interactive-p)
			    (x-face-Y-or-n-p "Is this X-Face type RGB? ")))
		   (insert (apply 'format
				  "X-Face-Type: RGB; geometry=%dx%d\n"
				  (if (eq 1 height)
				      '(1 1)
				    (list width (/ height 3)))))
		 (if (and (interactive-p)
			  (x-face-Y-or-n-p "Is this X-Face type Animate? "))
		     (progn
		       (setq rate (read-minibuffer "rate= " "0.5"))
		       (insert
			(format "X-Face-Type: animate=%g;geometry=%dx%d\n"
				rate width 1)))
		   (insert (format "X-Face-Type: geometry=%dx%d\n"
				   width height))))
	       (and (featurep 'xemacs)
		    (setq file (expand-file-name file)
			  cache (assoc file x-face-xmas-image-file-cache)
			  modtime (cadr cache)
			  size (caddr cache))
		    (put-text-property (1- (point)) (point)
				       'x-face-file-attributes
				       (list file modtime size))))
	     (and x-face-add-x-face-version-header
		  (goto-char pt1)
		  (x-face-insert-version-header))
	     (goto-char pt1)
	     (run-hooks 'x-face-insert-hook)
	     (if (interactive-p)
		 (progn
		   (and x-face-xmas-auto-image
			(not (and (listp x-face-xmas-auto-image)
				  (memq major-mode x-face-xmas-auto-image)))
			(x-face-xmas-display-x-face 1))
		   (setq x-face-xmas-last-highlight-headers-hack-x-face-p
			 'unknown))
	       (and (featurep 'xemacs)
		    (setq x-face-xmas-last-highlight-headers-hack-x-face-p
			  (x-face-xmas-highlight-headers-hack-x-face-p-p))))
	     (and (boundp 'mew-draft-buffer-header)
		  (markerp mew-draft-buffer-header)
		  (setq mew-draft-buffer-header (point-max-marker)))
	     (message msg "done")
	     (and (boundp 'mew-marker-header-end)
		  (markerp mew-marker-header-end)
		  (set-marker mew-marker-header-end (point-max))))))))

;;;###autoload
(defun x-face-menu-insert ()
  "Insert an X-Face using menu."
  (interactive)
  (and (> (string-to-int emacs-version) 18)
       (let ((minibuffer-setup-hook minibuffer-setup-hook)
	     (winconf (current-window-configuration))
	     x-face-default-xbm-file)
	 (add-hook 'minibuffer-setup-hook 'x-face-minibuffer-completion-help)
	 (call-interactively 'x-face-insert)
	 (set-window-configuration winconf))))

;;;###autoload
(defun x-face-save (&optional arg)
  "Extract X-Face headers in the current buffer, encode them to XBM data,
and save them in the local system.  Directory where XBM files will be
saved in is specified by the `x-face-image-file-directory-for-save'
variable.  You can use this command in a summary buffer rather than an
article buffer for some mail softwares, e.g. Gnus, Wanderlust, etc."
  (interactive "P")
  (let ((x-face-icons-to-color-picture-function
	 (if arg
	     (x-face-read-image-format)
	   x-face-icons-to-color-picture-function)))
    (x-face-save-or-view 'save)))

;;;###autoload
(defun x-face-view (&optional arg)
  "Extract X-Face headers in the current buffer, encode them to XBM data,
and show them using an external image viewer.  You can use this
command in a summary buffer rather than an article buffer for some
mail softwares, e.g. Gnus, Wanderlust, etc."
  (interactive "P")
  (let ((x-face-icons-to-color-picture-function
	 (if arg
	     (x-face-read-image-format)
	   x-face-icons-to-color-picture-function)))
    (x-face-save-or-view 'view)))

;;;###autoload
(defun x-face-ascii-view ()
  "Extract X-Face headers in the current buffer, encode them to XBM data,
and show them as ascii pictures in an Emacs frame."
  (interactive)
  (x-face-save-or-view 'ascii))


;;; x-face-mule
(defun x-face-snatch-x-face-mule-original-x-face-fields ()
  "Snatching at the x-face-mule's resources."
  (let ((xfaces (or (and (boundp 'x-face-mule-original-x-face-fields)
			 x-face-mule-original-x-face-fields)
		    (and (boundp 'x-face-mule:original-x-face-fields)
			 x-face-mule:original-x-face-fields)))
	(from (or (and (boundp 'x-face-mule-original-from-field)
		       x-face-mule-original-from-field)
		  (and (boundp 'x-face-mule:original-from-field)
		       x-face-mule:original-from-field))))
    (and xfaces
	 (save-excursion
	   (setq x-face-extracted-headers-buffer
		 (set-buffer (get-buffer-create " *header*")))
	   (setq buffer-read-only nil)
	   (erase-buffer)
	   (if from
	       (insert from)
	     (and (setq from (x-face-mail-fetch-mail-address))
		  (insert "From: " from "\n")))
	   (when (car xfaces)
	     (insert (car xfaces))
	     (or (bolp) (insert "\n")))
	   (mapcar (function (lambda (xface)
			       (insert xface)
			       (or (bolp) (insert "\n"))))
		   (cadr xfaces)))
	 x-face-extracted-headers-buffer)))

;; Pickup the macro `gnus-summary-article-number'.
(eval-when-compile
  (condition-case nil
      (load "gnus-sum" t)
    (error nil)))

;;; GNUS, Gnus
(defun x-face-gnus-get-article ()
  (or (and (boundp 'gnus-original-article-buffer)
	   (x-face-buffer-live-p gnus-original-article-buffer)
	   (eq gnus-current-article
	       (or (condition-case nil
		       (gnus-summary-article-number)
		     (invalid-function nil));; It maybe unexpanded macro.
		   (condition-case nil
		       ;; Attempt to eval the macro as a function.
		       (eval
			(funcall
			 (cdr (symbol-function 'gnus-summary-article-number))))
		     (error nil)))))
      (cond ((eq 'gnus-summary-mode major-mode)
	     (if (boundp 'gnus-original-article-buffer)
		 ;; >= v5.2
		 (gnus-summary-show-article)
	       ;; <= v5.1
	       (let ((gnus-show-all-headers t)
		     gnus-show-mime)
		 (gnus-summary-show-article))))
	    ((eq 'gnus-Subject-mode major-mode)
	     ;; <= 3.14.4
	     (let ((gnus-show-all-headers t))
	       (gnus-Subject-show-article))))))

(defun x-face-find-gnus-article-buffer ()
  "Find an article buffer which may includes X-Face headers."
  (or (save-excursion
	(set-buffer (or (and (boundp 'gnus-article-buffer)
			     (x-face-buffer-live-p gnus-article-buffer))
			(and (boundp 'gnus-Article-buffer)
			     (x-face-buffer-live-p gnus-Article-buffer))))
	(x-face-snatch-x-face-mule-original-x-face-fields))
      (and (boundp 'gnus-original-article-buffer)
	   (x-face-buffer-live-p gnus-original-article-buffer))
      (and (boundp 'gnus-article-buffer)
	   (x-face-buffer-live-p gnus-article-buffer))
      (and (boundp 'gnus-Article-buffer)
	   (x-face-buffer-live-p gnus-Article-buffer))))

(defun x-face-gnus-unwind-after-save-or-view ()
  (or (boundp 'gnus-original-article-buffer)
      (cond ((eq 'gnus-summary-mode major-mode)
	     (gnus-summary-show-article))
	    ((eq 'gnus-Subject-mode major-mode)
	     (gnus-Subject-show-article)))))

;;; MH-E
(defun x-face-mh-get-article ()
  "Get current message in article buffer."
  (or (and (x-face-buffer-live-p mh-show-buffer)
	   (save-excursion
	     (set-buffer mh-show-buffer)
	     (x-face-snatch-x-face-mule-original-x-face-fields)))
      (let ((file (concat mh-folder-filename (mh-get-msg-num t))))
	(save-excursion
	  (setq x-face-extracted-headers-buffer
		(set-buffer (get-buffer-create " *header*")))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert-file-contents file)
	  (x-face-delcr)
	  (and (search-forward "\n\n" nil t)
	       (delete-region (1- (point)) (point-max)))))))

;;; Mew
(eval-when-compile
  (require 'mew)
  (require 'mew-cache nil t)
  (require 'mew-syntax nil t)
  ;; Avoid macro `mew-cache-hit' expand (Mew 1.94.2 or earlier).
  ;; Changed `mew-cache-hit' from macro to function at Mew 2.0.
  (if (not (fboundp 'mew-current-get-fld))
      (setq byte-compile-macro-environment
	    (cons '(mew-cache-hit . nil) byte-compile-macro-environment))))

(defun x-face-mew-get-article ()
  "Get current message in article buffer."
  (or (let ((buffer (mew-buffer-message)))
	(and (x-face-buffer-live-p buffer)
	     (save-excursion
	       (set-buffer buffer)
	       (and (mew-header-get-value "x-face:")
		    (x-face-snatch-x-face-mule-original-x-face-fields)))))
      (let* ((file (cond
		    ((fboundp 'mew-current-get-fld)
		     (mew-expand-folder (mew-current-get-fld (mew-frame-id))
					(mew-current-get-msg (mew-frame-id))))
		    ((fboundp 'mew-expand-folder)
		     (mew-expand-folder (mew-summary-folder-name)
					(mew-summary-message-number)))
		    (t
		     (mew-expand-file-name (mew-summary-message-number)
					   (mew-summary-folder-name)))))
	     (buf (cond
		   ((fboundp 'mew-current-get-fld)
		    (mew-cache-hit (mew-current-get-fld (mew-frame-id))
				   (mew-current-get-msg (mew-frame-id))
				   'must-hist))
		   (t
		    (mew-current-get 'cache))))
	     (part (cond
		    ((fboundp 'mew-syntax-nums)
		     (or (mew-syntax-nums) '(1)))
		    ((fboundp 'mew-syntax-number)
		     (mew-syntax-number))
		    (t
		     (mew-current-get 'part))))
	     (syntax (cond
		      ((fboundp 'mew-syntax-get-entry)
		       (mew-syntax-get-entry
			(mew-cache-decode-syntax buf) part))
		      (t
		       (mew-syntax-get-entry-strnum
			(mew-cache-decode-syntax
			 (mew-cache-hit (mew-current-get 'message))) part))))
	     (beg (mew-syntax-get-begin syntax))
	     (end (mew-syntax-get-end   syntax)))
	(save-excursion
	  (setq x-face-extracted-headers-buffer
		(set-buffer (get-buffer-create " *header*")))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (cond
	   ((and part (not (equal part '(1))))
	    (insert-buffer-substring buf beg end))
	   (t
	    (insert-file-contents file nil 0
				  (* (if (boundp 'mew-header-reasonable-size)
					 mew-header-reasonable-size 5000) 2))
	    (x-face-delcr)
	    (and (search-forward "\n\n" nil t)
		 (delete-region (1- (point)) (point-max)))))))))

;;; RMAIL, VM
(defun x-face-get-all-headers-in-article-buffer ()
  (unless (x-face-snatch-x-face-mule-original-x-face-fields)
    (goto-char (point-min))
    (let ((end (if (search-forward "\n\n" nil t)
		   (point)
		 (point-max)))
	  headers)
      (goto-char (point-min))
      (save-restriction
	(widen)
	(goto-char
	 (if (search-backward "\n\n" nil t)
	     (match-end 0)
	   (point-min)))
	(setq headers (buffer-substring-no-properties (point) end)))
      (setq x-face-extracted-headers-buffer
	    (set-buffer (get-buffer-create " *header*")))
      (erase-buffer)
      (insert headers))))

(defun x-face-rmail-get-all-headers ()
  "Get all headers in article buffer."
  (save-excursion
    (x-face-get-all-headers-in-article-buffer)))

(eval-when-compile
  (condition-case nil
      (require 'vm-macro) ; >= VM 6.63
    (error nil))
  (condition-case nil
      (require 'vm-misc) ; <= VM 6.62
    (error nil)))

(defun x-face-vm-get-all-headers (msg)
  "Get all headers in article buffer."
  (vm-follow-summary-cursor)
  (message msg)
  (save-excursion
    (vm-select-folder-buffer)
    (x-face-get-all-headers-in-article-buffer)))

;;; cmail
(defun x-face-cmail-get-article ()
  "Get current message in article buffer."
  (let ((page (cmail-get-page-number-from-summary))
	(folder (get-buffer (cmail-folder-buffer cmail-current-folder)))
	beg end)
    (save-excursion
      (set-buffer folder)
      (unless (x-face-snatch-x-face-mule-original-x-face-fields)
	(cmail-n-page page)
	(setq beg (point))
	(cmail-n-page (1+ page))
	(forward-line -1)
	(setq end (point))
	(goto-char beg)
	(and (re-search-forward "\\(\n\r?\\)\n" end t)
	     (setq end (match-end 1)))
	(setq x-face-extracted-headers-buffer
	      (set-buffer (get-buffer-create " *header*")))
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert-buffer-substring folder beg end)
	(x-face-delcr)))))

;;; Wanderlust
(defun x-face-wl-get-article ()
  "Get current message in article buffer."
  (save-excursion
    (wl-summary-set-message-buffer-or-redisplay)
    (let ((message-buf (wl-message-get-original-buffer)))
      (setq x-face-extracted-headers-buffer
	    (set-buffer (get-buffer-create " *header*")))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-buffer-substring message-buf))))


;;; Asynchronous view functions.
(defun x-face-view-asynchronous-function-for-OS/2 (file)
  "Asynchronous view function for OS/2."
  (unwind-protect
      (progn
	(call-process "cmd.exe" nil nil nil "/c"
		      (mapconcat
		       'identity
		       (append (list "detach" x-face-viewer-program)
			       x-face-viewer-options
			       (list file))
		       " "))
	(sleep-for x-face-viewer-startup-time))
    (delete-file file)
    (delete-directory (file-name-directory file))))

(defun x-face-sentinel-delete-file (process event)
  (let* ((list (assq process x-face-delete-file-alist))
	 (file (cdr list)))
    (when (and file
	       (file-exists-p file))
      (delete-file file)
      (delete-directory (file-name-directory file)))
    (setq x-face-delete-file-alist (delq list x-face-delete-file-alist))))

(defun x-face-view-asynchronous-function-for-win95 (file)
  "Asynchronous view function for Windoze."
  (let ((buffer (get-buffer-create "*X-Face-process*"))
	(msg (format "Executing..: %s %s" x-face-viewer-program file)))
    (unwind-protect
	(progn
	  (save-excursion
	    (x-face-set-buffer-disable-undo buffer)
	    (message "%s" msg)
	    (insert msg "\n\n")
	    (let (process)
	      (setq process (start-process x-face-viewer-program buffer
					   x-face-viewer-program file))
	      (setq x-face-delete-file-alist
		    (cons (cons process file) x-face-delete-file-alist))
	      (set-process-sentinel process 'x-face-sentinel-delete-file)))))))

(defun x-face-view-asynchronous-function-for-some (file)
  "Asynchronous view function for some UN|X systems."
  (call-process
   "/bin/sh" nil nil nil "-c"
   (mapconcat
    'identity
    (append (list "(" "/bin/cat" file "|" x-face-viewer-program)
	    x-face-viewer-options
	    (list ")>/dev/null 2>&1 </dev/null&"))
    " "))
  (call-process "/bin/sh" nil nil nil "-c"
		(format
		 "(/bin/sleep %d;/bin/rm -fr %s)>/dev/null 2>&1 </dev/null&"
		 x-face-viewer-startup-time
		 (file-name-directory file))))


;;; Menu.
(and (not (featurep 'xemacs))
     (> (string-to-int emacs-version) 18)
     (progn

       (defconst x-face-menu-title "X-Face")

       (defvar x-face-menu-map (make-sparse-keymap x-face-menu-title))

       (defconst x-face-menu-list
	 '(("View"
	    x-face-view
	    (zerop (minibuffer-depth)))
	   ("View Ascii"
	    x-face-ascii-view
	    (zerop (minibuffer-depth)))
	   ("Encode"
	    x-face-menu-encode
	    (zerop (minibuffer-depth)))
	   ("Insert"
	    x-face-menu-insert
	    (and (not buffer-read-only)
		 (zerop (minibuffer-depth))))
	   ("Select XFace"
	    select-xface
	    (and (fboundp (quote select-xface))
		 (not buffer-read-only)
		 (zerop (minibuffer-depth))))
	   ("Save"
	    x-face-save
	    (zerop (minibuffer-depth)))))

       (mapcar
	(function
	 (lambda (item)
	   (let ((name (car item))
		 (command (cadr item))
		 (enablep (caddr item)))
	     (define-key x-face-menu-map (vector (intern name))
	       (cons name (cons '(nil) command)))
	     (put command 'menu-enable enablep))))
	(reverse x-face-menu-list))

       (defun x-face-define-menu ()
	 "Define menu for Mule 2.3, Emacs 20.x."
	 (let ((map (current-local-map))
	       (flag menu-bar-mode))
	   (if map
	       (progn
		 (menu-bar-mode 1)
		 (define-key map (vector 'menu-bar (intern x-face-menu-title))
		   (cons x-face-menu-title x-face-menu-map))
		 (or flag (menu-bar-mode -1))
		 (redraw-display))
	     (message "!! Local keymap does not exists.")
	     (sit-for 1))))))


;;; Splash screen.
(eval-and-compile
  (unless (featurep 'xemacs)
    (load "bitmap" t)))

(eval-when-compile
  (unless (featurep 'xemacs)
    (defvar filename)
    (defmacro x-face-image-bitmap-logo ()
      (and
       (featurep 'bitmap)
       (let ((dir (if (and (boundp 'filename)
			   (stringp filename)
			   (file-exists-p filename))
		      (file-name-directory filename)
		    default-directory))
	     xbm logo warn)
	 (setq xbm (expand-file-name
		    (if (string-match "(\\(alpha\\|beta\\)[0-9]+)$"
				      x-face-version-number)
			"x-face-beta-logo.xbm"
		      "x-face-logo.xbm")
		    dir))
	 (if (file-exists-p xbm)
	     (unless (setq logo
			   (condition-case ()
			       (bitmap-decode-xbm (bitmap-read-xbm-file xbm))
			     (error nil)))
	       (setq warn "Warning: cannot create image data."))
	   (setq warn (format "Warning: file \"%s\" not found." xbm)))
	 (and warn
	      (if (fboundp 'byte-compile-warn)
		  (byte-compile-warn "%s" warn)
		(message "%s" warn)))
	 logo)))))

(unless (featurep 'xemacs)
  (defconst x-face-image-bitmap-logo
    (when (featurep 'bitmap)
      (let ((cmp (x-face-image-bitmap-logo)))
	(when cmp
	  (condition-case ()
	      (let ((len (length cmp))
		    (bitmap (bitmap-compose (aref cmp 0)))
		    (i 1))
		(while (< i len)
		  (setq bitmap (concat bitmap "\n"
				       (bitmap-compose (aref cmp i)))
			i (1+ i)))
		bitmap)
	    (error nil))))))

  (defun x-face-bitmap-splash-at-point ()
    (or (bolp) (insert "\n"))
    (let ((bow (point))
	  (width 0)
	  (fill-column (window-width))
	  eoi height vmargin hmargin)
      (insert x-face-image-bitmap-logo)
      (or (bolp) (insert "\n"))
;;      (and (fboundp 'facep)
;;	   (facep 'gnus-splash-face)
;;	   (put-text-property bow (point) 'face 'gnus-splash-face))
      (setq eoi (point)
	    height (count-lines bow eoi)
	    vmargin (max 0 (- (window-height) 2 height)))
      (goto-char bow)
      (while (< (point) eoi)
	(end-of-line)
	(setq width (max width (current-column)))
	(forward-line 1))
      (and (> vmargin 0) (insert "\n"))
      (insert x-face-version "\n")
;;      (and (fboundp 'facep)
;;	   (facep 'bold-italic)
;;	   (put-text-property eoi (point) 'face 'bold-italic))
      (center-region eoi (point))
      (setq hmargin (max 0 (/ (- fill-column width) 2)))
      (insert-char ?\n (/ vmargin 2))
      (goto-char eoi)
      (forward-line -1)
      (while (> (point) bow)
	(insert-char ?\  hmargin)
	(forward-line -1))
      (goto-char bow)
      (insert-char ?\  hmargin)
      (goto-char bow)
      (insert-char ?\n (/ (max 0 (1- vmargin)) 2))
      (goto-char bow)
      (set-window-start (get-buffer-window (current-buffer)) bow)))

  (defun x-face-bitmap-splash (&optional arg)
    (interactive "P")
    (and x-face-image-bitmap-logo
	 (let (config buffer)
	   (unwind-protect
	       (progn
		 (setq config (current-window-configuration))
		 (switch-to-buffer
		  (setq buffer (generate-new-buffer
				(concat (if arg "*" " *")
					x-face-version "*"))))
		 (delete-other-windows)
		 (x-face-bitmap-splash-at-point)
		 (set-buffer-modified-p nil)
		 (sit-for 0)
		 (or arg (sleep-for 2)))
	     (unless arg
	       (kill-buffer buffer)
	       (set-window-configuration config)
	       (sit-for 0))))))

  (or x-face-inhibit-loadup-splash
      (and window-system (featurep 'bitmap) (x-face-bitmap-splash))))


(provide 'x-face)
(run-hooks 'x-face-load-hook)

(eval-when-compile
  (and (featurep 'xemacs)
       (setq load-path (cons (if (and (boundp 'filename)
				      (stringp filename)
				      (file-exists-p filename))
				 (file-name-directory filename)
			       default-directory)
			     load-path))))

(and (featurep 'xemacs) (require 'x-face-xmas))

;;; x-face.el ends here
