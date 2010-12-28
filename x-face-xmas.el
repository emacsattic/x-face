;;; x-face-xmas.el -- X-Face functions for XEmacs.

;; Copyright (C) 1996-2002 Katsumi Yamaoka
;; Copyright (C) 1996-2002 Ken'ichi OKADA
;; Copyright (C) 1996-2002 Yuuichi Teranishi
;; Copyright (C) 2002      Daiki Ueno
;; Author: Katsumi Yamaoka   <yamaoka@jpl.org>
;;         Ken'ichi OKADA    <okada@opaopa.org>
;;         Yuuichi Teranishi <teranisi@gohome.org>
;;         Daiki Ueno        <ueno@unixuser.org>
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 1996/09/19
;; Revised: 2002/02/15

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

;; Commands:
;;  x-face-xmas-display-x-face
;;     "Display X-Face fields as XEmacs glyph."

;;; Code:

(eval-when-compile
  (defvar filename)
  (setq load-path (cons (if (and (boundp 'filename)
				 (stringp filename)
				 (file-exists-p filename))
			    (file-name-directory filename)
			  default-directory)
			load-path)))
(require 'x-face)

;; Silence the byte compiler.
(eval-when-compile
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
	 '(*cmail-mail-buffer
	   gnus-article-buffer
	   gnus-current-headers
	   highlight-headers-hack-x-face-p
	   last
	   mime-display-header-hook
	   mime-module-version
	   mime-preview-original-major-mode
	   mime-raw-buffer
	   mime-view-content-header-filter-hook
	   mime-view-original-major-mode
	   mime-viewer/content-header-filter-hook
	   mime::preview/original-major-mode
	   original-major-mode
	   orig::mime-display-header-hook
	   orig::mime-view-content-header-filter-hook
	   orig::mime-viewer/content-header-filter-hook
	   semi-version))
      (make-local-variable 'byte-compile-warnings)
      (setq byte-compile-warnings nil))))

(defgroup x-face-xmas nil
  "X-Face utility for XEmacs."
  :prefix "x-face-xmas-"
  :group 'x-face)

(defcustom x-face-xmas-auto-image t
  "Controls whether display image after 'x-face-insert interactively.
t means always ON, nil for OFF, and list of MODEs means OFF if the current
major-mode matches one."
  :group 'x-face
  :group 'x-face-xmas
  :type 'boolean)

(defcustom x-face-xmas-inhibit-read-only nil
  "Non-nil forces don't put read-only text-property to the message header."
  :group 'x-face
  :group 'x-face-xmas
  :type 'boolean)

(defcustom x-face-xmas-like-highlight-headers nil
  "Non-nil means highlight-headers' style is applied for displaying faces."
  :group 'x-face
  :group 'x-face-xmas
  :type 'boolean)

(defconst x-face-xmas-xface-p
  (and (featurep 'xface)
       (featurep 'xpm))
  "Non-nil means this XEmacs has 'xface feature.")

(when x-face-xmas-xface-p

  (defface x-face-xmas-x-face-face
    (list '(((class color) (background dark))
	    (:foreground "Black" :background "White"))
	  (list t (list ':foreground "Black"
			':background (color-name (face-background 'default)))))
    "Face to show X face."
    :group 'x-face
    :group 'x-face-xmas)

  (defface x-face-xmas-x-face-face-for-dialog
    '((t (:foreground "#006400" :background "#fffacd")))
    "Face to show X face in the dialog box."
    :group 'x-face
    :group 'x-face-xmas))

(defcustom x-face-xmas-dialog-frame-alist
  '((width . 48) (height . 16) (left . 0) (top . -1))
  "Plist of frame properties for a dialog frame."
  :group 'x-face
  :group 'x-face-xmas
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

(defcustom x-face-xmas-font-height-in-dialog-box 16
  "Height of the font in dialog box."
  :group 'x-face
  :group 'x-face-xmas
  :type 'integer)

(defcustom x-face-xmas-image-xpm-filebad
  "/* XPM */
static char *filebad_xpm[] = {
/* width height num_colors chars_per_pixel */
\"20 16 4 1\",
/* colors */
\"  s None c None\",
\". c #ffffff\",
\"X c #ff0000\",
\"# c #000000\",
/* pixels */
\"##############      \",
\"#............##     \",
\"#...XX.......#.#    \",
\"#....XX......#..#   \",
\"#.....XX....X#...#  \",
\"#......XX..XX###### \",
\"#.......XXXX.#######\",
\"#........XX.......##\",
\"#.......XXXX......##\",
\"#......XX..XX.....##\",
\"#.....XX....XX....##\",
\"#....XX......XX...##\",
\"#...XX........XX..##\",
\"#.................##\",
\"####################\",
\" ###################\"
};
"
  "Thumbnail image for bad file."
  :group 'x-face
  :group 'x-face-xmas
  :type 'string)

(defcustom x-face-xmas-image-xpm-filebad-height 16
  "Height of thumbnail image for bad file."
  :group 'x-face
  :group 'x-face-xmas
  :type 'integer)

(defcustom x-face-xmas-load-hook nil
  "Hook to be run after the x-face package has been loaded."
  :group 'x-face
  :group 'x-face-xmas
  :type 'hook)

(defcustom x-face-animate-limit-intervals '(0.1 . 2)
  "Cons of minimum and maximum interval time."
  :group 'x-face
  :group 'x-face-xmas
  :type '(cons (number :tag "min.") (number :tag "max.")))


;;; Internal variables.
(make-variable-buffer-local
 (defvar x-face-xmas-last-highlight-headers-hack-x-face-p 'unknown))

(defvar x-face-xmas-image-file-cache nil
  "((filename modtime filesize ((height . glyph) (height . glyph)) ...)")

(defvar x-face-xmas-image-field-cache nil
  "((field glyph) (field glyph) ...)")

(defvar x-face-xmas-animation-id 1)

(defvar x-face-xmas-animation-keymap nil)
(if (null x-face-xmas-animation-keymap)
    (setq x-face-xmas-animation-keymap (make-sparse-keymap)))
(define-key x-face-xmas-animation-keymap 'button1 'x-face-xmas-animate-toggle)
(define-key x-face-xmas-animation-keymap 'button2 'x-face-xmas-animate-toggle)

(and (featurep 'xpm)
     (eval-when-compile
       (defmacro x-face-xmas-image-xpm-logo ()
	 (let ((logo (if (string-match "(\\(alpha\\|beta\\)[0-9]+)$"
				       x-face-version-number)
			 "x-face-beta-logo.xpm"
		       "x-face-logo.xpm"))
	       (dir (if (and (boundp 'filename)
			     (stringp filename)
			     (file-exists-p filename))
			(file-name-directory filename)
		      default-directory)))
	   (setq logo (expand-file-name logo dir))
	   (if (file-exists-p logo)
	       (let ((buffer (generate-new-buffer " *x-face-logo*"))
		     (coding-system-for-read (quote binary))
		     buffer-file-format format-alist
		     insert-file-contents-post-hook
		     insert-file-contents-pre-hook)
		 (prog1
		     (save-excursion
		       (set-buffer buffer)
		       (insert-file-contents logo)
		       (buffer-string))
		   (kill-buffer buffer)))
	     (progn
	       (require 'bytecomp)
	       (byte-compile-warn
		"Warning: file \"%s\" not found." logo)
	       (sit-for 2)
	       nil))))))

(defconst x-face-xmas-image-xpm-logo
  (and (featurep 'xpm)
       (x-face-xmas-image-xpm-logo)))

(defun x-face-xmas-highlight-headers-hack-x-face-p-p ()
  (if (boundp 'highlight-headers-hack-x-face-p)
      (and highlight-headers-hack-x-face-p t)
    t))


;;; Macros and defsubsts.

;;(defmacro x-face-xmas-highlight-headers-hack-x-face-p-p ()
;;  '(if (boundp (quote highlight-headers-hack-x-face-p))
;;       (and highlight-headers-hack-x-face-p t)
;;     t))

(defmacro x-face-xmas-modified-dialog-frame-plist ()
  '(let ((olist dialog-frame-plist)
	 (items (quote (height left top)))
	 (replaces (quote (width)))
	 new value elt plist)
     (setq new
	   (mapcar
	    (lambda (item)
	      (and (setq value
			 (cdr (assq item x-face-xmas-dialog-frame-alist)))
		   (setq replaces (push item replaces))
		   (list item value)))
	    items))
     (while (setq elt (pop olist))
       (if (memq elt replaces)
	   (pop olist)
	 (setq plist (append plist (list elt (pop olist))))))
     (apply (quote append) (quote (width 18)) plist new)))


;;; Functions and/or Comnnands.

;;;###autoload
(defun x-face-menu-encode ()
  "Generate X-Face string(s) from xbm file with menu."
  (interactive)
  (let ((default-directory (x-face-file-name-as-directory
			    x-face-image-file-directory))
	(force force-dialog-box-use)
	(dialog-frame-plist (x-face-xmas-modified-dialog-frame-plist))
	(window-min-width 1)
	(completion-setup-hook completion-setup-hook)
	(orig (symbol-function
	       'mouse-directory-display-completion-list))
	(frame-title-format (list 'x-face-completions
				  "*X-Face Encode*"
				  frame-title-format))
	(x-face-completions t)
	x-face-default-xbm-file)
    (add-hook 'completion-setup-hook
	      'x-face-xmas-display-x-face-in-dialog-box)
    (unwind-protect
	(progn
	  (setq force-dialog-box-use t)
	  (fset 'mouse-directory-display-completion-list x-face-nop)
	  (call-interactively 'x-face-encode))
      (setq force-dialog-box-use force)
      (fset 'mouse-directory-display-completion-list orig))))

;;;###autoload
(defun x-face-menu-insert ()
  "Insert X-Face with menu."
  (interactive)
  (let ((default-directory (x-face-file-name-as-directory
			    x-face-image-file-directory))
	(force force-dialog-box-use)
	(dialog-frame-plist (x-face-xmas-modified-dialog-frame-plist))
	(window-min-width 1)
	(completion-setup-hook completion-setup-hook)
	(orig (symbol-function
	       'mouse-directory-display-completion-list))
	(frame-title-format (list 'x-face-completions
				  "*X-Face Insert*"
				  frame-title-format))
	(x-face-completions t)
	x-face-default-xbm-file)
    (add-hook 'completion-setup-hook
	      'x-face-xmas-display-x-face-in-dialog-box)
    (unwind-protect
	(progn
	  (setq force-dialog-box-use t)
	  (fset 'mouse-directory-display-completion-list x-face-nop)
	  (call-interactively 'x-face-insert))
      (setq force-dialog-box-use force)
      (fset 'mouse-directory-display-completion-list orig))))

;;;###autoload
(defun x-face-xmas-wl-display-x-face (&optional beg end arg)
  "A function to display X-Face for Wanderlust."
  (let ((x-face-xmas-inhibit-read-only t))
    (x-face-xmas-display-x-face (if arg -1 1) beg end)))

(defalias 'x-face-xmas-mew-display-x-face 'x-face-xmas-wl-display-x-face)

;;;###autoload
(defun x-face-xmas-cmail-display-x-face (&optional arg)
  (x-face-xmas-display-x-face-not-read-only *cmail-mail-buffer arg))

(defun x-face-xmas-extract-x-face-fields-to-glyphs ()
  (let (field animate type x y glyph)
    (mapcar
     (lambda (icons)
       (setq field (pop icons))
       (if (or (glyphp icons)
	       (numberp (car icons))) ; animated glyph
	   icons
	 (setq type (pop icons)
	       x (pop icons)
	       y (pop icons)
	       animate (pop icons)
	       glyph nil)
	 (setq glyph
	       (if animate
		   (cons animate
			 (mapcar '(lambda (icon)
				    (x-face-xmas-extract-x-face-fields-internal
				     type x y icon))
				 (x-face-xmas-pack-icons
				  icons (apply '* (if (eq 'rgb type) 3 1)
					       (list x y)))))
		 (x-face-xmas-extract-x-face-fields-internal type x y icons)))
	 (and field (push (list field glyph) x-face-xmas-image-field-cache))
	 glyph))
     (save-excursion
       (x-face-extract-x-face-fields-to-icons nil 'glyph)))))

(defun x-face-xmas-extract-x-face-fields-internal (type x y icons)
  (let (glyph)
    (if (eq 'rgb type)
	(save-excursion
	  (set-buffer (x-face-icons-to-xpm
		       "noname" x y
		       (x-face-concat-rgb-icons x y icons)))
	  (setq glyph (make-glyph
		       (vector 'xpm :data (buffer-string))))
	  (kill-buffer (current-buffer)))
      (if (eq 'gray type)
	  (setq glyph (make-glyph
		       (vector 'xpm :data
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
				 icons)))))
	(setq glyph
	      (make-glyph
	       (vector
		'xbm :data
		(list
		 (* 48 x) (* 48 y)
		 (mapconcat 'char-to-string
			    (x-face-ascii-to-binary-icon
			     (x-face-concat-mono-icons x y icons)
			     nil nil 'xmas)
			    "")))))
	(set-glyph-face glyph 'x-face-xmas-x-face-face)))
    glyph))

(defsubst x-face-xmas-pack-icons (icons num)
  (let (ic)
    (while icons
      (setq ic
	    (append ic (list icons)))
      (setq icons
	    (prog1
		(nthcdr num icons)
	      (if (nthcdr (1- num) icons)
		  (setcdr (nthcdr (1- num) icons) nil)))))
    ic))

(defun x-face-xmas-search-extents ()
  (let ((case-fold-search t)
	points extent extents from x-face)
    (goto-char (point-min))
    (while (setq points (x-face-search-field "^From" t))
      (setq extent nil
	    extents nil)
      (while (setq extent (extent-at (cadr points) nil nil extent 'at))
	(and (or (extent-begin-glyph extent) (extent-end-glyph extent))
	     (extent-property extent 'x-face-image)
	     (setq extents (append extents (list extent)))))
      (setq from (append from (list (append points extents)))))
    (goto-char (point-min))
    (while (setq points (x-face-search-field "^X-Face\\(-[0-9]+\\)?" t))
      (setq extent nil
	    extents nil)
      (while (setq extent (extent-at (cadr points) nil nil extent 'at))
	(and (or (extent-begin-glyph extent) (extent-end-glyph extent))
	     (extent-property extent 'x-face-image)
	     (setq extents (append extents (list extent)))))
      (setq x-face (append x-face (list (append points extents)))))
;;    ((from   (P0 P1 P2 EXTENT EXTENT ...))
;;     (x-face (P0 P1 P2 EXTENT EXTENT ...)))
    (list (cons 'from from) (cons 'x-face x-face))))

;;;###autoload
(defun x-face-xmas-display-x-face (arg &optional beg end)
  "Display X-Face fields as XEmacs glyph."
  (interactive "P")
  (and
   x-face-xmas-xface-p
   (not (eq 'tty (device-type)))
   (save-excursion
     (save-restriction
       (apply 'narrow-to-region
	      (cond ((and beg end)
		     (list beg end))
		    ((and zmacs-regions (interactive-p) (region-exists-p))
		     (list (region-beginning) (region-end)))
		    (t (list (point-min) (point-max)))))
       (x-face-narrow-to-header)
       (let ((exists (x-face-xmas-search-extents))
	     (gnus-article-p (and (boundp 'gnus-article-buffer)
				  (eq (get-buffer gnus-article-buffer)
				      (current-buffer))))
	     (hide-props (if x-face-xmas-inhibit-read-only
			     '(invisible t)
			   '(invisible t read-only t)))
	     lives on x-face from pt extent glyphs)
	 ;; extract live extents.
	 (mapcar
	  (lambda (i) (setq lives (append lives (cdddr i))))
	  (append (cdar exists) (cdadr exists)))
	 (setq x-face (mapcar
		       (lambda (i) (list (car i) (cadr i) (caddr i)))
		       (cdr (assq 'x-face exists))))
	 ;; delete all extents.
	 (put-text-property (point-min) (point-max) 'read-only nil)
	 (mapcar 'delete-extent lives)
	 ;; whether ON or OFF?
	 (setq on
	       (and
		(x-face-xmas-highlight-headers-hack-x-face-p-p)
		x-face
		(or (not (or lives (and (numberp arg) (< arg 0))))
		    (and (numberp arg) (> arg 0)))))
	 ;;
	 (when on
	   (cond ((setq from (mapcar
			      (lambda (i) (list (car i) (cadr i) (caddr i)))
			      (cdr (assq 'from exists))))
		  (setq pt (cadar from))
		  (unless gnus-article-p
		    (mapcar
		     (lambda (i)
		       (add-text-properties (car i) (caddr i) hide-props))
		     x-face)))
		 ((and (not gnus-article-p) x-face)
		  (setq pt (cadar x-face))
		  (add-text-properties pt (1- (caddar x-face)) hide-props)
		  (when (memq 'read-only hide-props)
		    (put-text-property pt (caddar x-face) 'read-only t))
		  (mapcar
		   (lambda (i)
		     (add-text-properties (car i) (caddr i) hide-props))
		   (cdr x-face)))
		 (x-face;; There is no From field in `gnus-article-buffer'.
		  (goto-char (point-min))
		  (let ((inhibit-read-only t)
			buffer-read-only)
		    (insert "From: ")
		    (setq pt (point))
		    (insert "(unknown)\n"))))
	   (message "Decoding X-Face image...")
	   (setq glyphs (x-face-xmas-extract-x-face-fields-to-glyphs))
	   (if glyphs
	       (progn
		 (mapcar
		  (lambda (glyph)
		    (set-extent-properties
		     (setq extent (make-extent (1- pt) pt))
		     '(x-face-image t duplicable t))
		    (if (listp glyph) ;; animated glyph
			(let ((id (if (natnump x-face-xmas-animation-id)
				      x-face-xmas-animation-id
				    1)))
			  (setq x-face-xmas-animation-id (1+ id))
			  (set-extent-properties
			   extent
			   (list 'keymap x-face-xmas-animation-keymap
				 'x-face-xmas-animation-id id
				 'x-face-xmas-animation-interval
				 (max (car x-face-animate-limit-intervals)
				      (min (cdr x-face-animate-limit-intervals)
					   (car glyph)))
				 'x-face-xmas-animation-buffer (current-buffer)
				 'x-face-xmas-location
				 (and x-face-xmas-like-highlight-headers from)
				 'x-face-xmas-animation-glyphs (cdr glyph)))
			  (x-face-animate extent))
		      (if (and x-face-xmas-like-highlight-headers from)
			  (set-extent-end-glyph extent glyph)
			(set-extent-begin-glyph extent glyph))))
		  (if (and x-face-xmas-like-highlight-headers (not from))
		      (nreverse glyphs)
		    glyphs))
		 (message ""))
	     (message "Decoding X-Face image...!! FAILED")
	     (setq on nil)))
	 (if gnus-article-p
	     (setq last on)
	   (unless on
	     (mapcar
	      (lambda (i)
		(put-text-property (car i) (caddr i) 'invisible nil))
	      x-face))))))))

(defun x-face-animate (extent)
  "Animate X-Face."
  (let ((buffer (extent-property extent 'x-face-xmas-animation-buffer))
	(id (extent-property extent 'x-face-xmas-animation-id)))
  (when (x-face-buffer-live-p buffer)
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(x-face-narrow-to-header)
	(unless (and (extent-live-p extent)
		     (not (extent-detached-p extent)))
	  (setq extent nil)
	  (map-extents
	   (lambda (e maparg)
	     (when (eq id (extent-property e 'x-face-xmas-animation-id))
	       (setq extent e)))))
	(when extent
	  (when (let ((window (get-buffer-window buffer)))
		  (and window
		       (pos-visible-in-window-p (extent-start-position extent)
						window)))
	    (let* ((glyphs (extent-property extent
					    'x-face-xmas-animation-glyphs))
		   (glyph (car glyphs)))
	      (if (extent-property extent 'x-face-xmas-location)
		  (set-extent-end-glyph extent glyph)
		(set-extent-begin-glyph extent glyph))
	      (set-extent-property extent 'x-face-xmas-animation-glyphs
				   (nconc (cdr glyphs) (list glyph)))))
	  (set-extent-property
	   extent
	   'x-face-xmas-animation-timer
	   (run-with-timer (extent-property extent
					    'x-face-xmas-animation-interval)
			   nil 'x-face-animate extent))))))))

(defun x-face-xmas-animate-toggle (event)
  "Toggle Animation of X-Face."
  (interactive "e")
  (let ((timer (extent-property (event-glyph-extent event)
				'x-face-xmas-animation-timer)))
    (if (memq timer timer-list)
	(cancel-timer timer)
      (x-face-animate (event-glyph-extent event)))))

;;;###autoload
(defun x-face-xmas-force-display-x-face ()
  "Display X-Face fields compulsorily as XEmacs glyph."
  (x-face-xmas-display-x-face 1))

;;;###autoload
(defun x-face-xmas-display-x-face-not-read-only (&optional buffer arg)
  "Display X-Face in the specified buffer."
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (let ((x-face-xmas-inhibit-read-only t))
      (x-face-xmas-display-x-face (if arg -1 1)))))

;;;###autoload
(defun x-face-xmas-remove-x-face-glyph ()
  "Remove X-Face images and some text-properties."
  (x-face-xmas-display-x-face -1))

(defun x-face-xmas-highlight-headers (start end hack-sig)
  "Replacement of highlight-headers."
  (let ((x-face-xmas-inhibit-read-only t)
	;;(x-face-xmas-like-highlight-headers t)
	(highlight-headers-hack-x-face-p
	 (or highlight-headers-hack-x-face-p
	     (eq t x-face-xmas-last-highlight-headers-hack-x-face-p)))
	s extent extents)
    (setq x-face-xmas-last-highlight-headers-hack-x-face-p 'unknown)
    (and (< end start) (setq s start
			     start end
			     end s))
    (orig::highlight-headers start end hack-sig)
    (save-excursion
      (save-restriction
	(widen)
	(and hack-sig
	     (goto-char end)
	     (re-search-backward "\n--+ *\n" start t)
	     (setq end (1+ (point))))
	(goto-char start)
	(when (re-search-forward "^From: *" end t)
	  (while (setq extent (extent-at (point) nil nil extent 'at))
	    (and (extent-end-glyph extent)
		 (push extent extents)))
	  (mapcar 'delete-extent extents))
	(setq extent nil
	      extents nil)
	(goto-char start)
	(when (re-search-forward "^X-Face:" end t)
	  (while (setq extent (extent-at
			       (match-beginning 0) nil nil extent 'at))
	    (and (extent-property extent 'invisible)
		 (push extent extents)))
	  (mapcar 'delete-extent extents))))
    (x-face-xmas-display-x-face 1 start end)))

(defun x-face-xmas-replace-highlight-headers (&optional arg)
  "Replace highlight-headers."
  (unless (fboundp 'orig::highlight-headers)
    (require 'highlight-headers)
    (fset 'orig::highlight-headers
	  (symbol-function 'highlight-headers)))
  (if arg
      (fset 'highlight-headers
	    (symbol-function 'orig::highlight-headers))
    (fset 'highlight-headers
	  (symbol-function 'x-face-xmas-highlight-headers))))

(defun x-face-xmas-mime-highlight-headers ()
  "Replacement of mime-preview-x-face-function-use-highlight-headers."
  (or (and (boundp 'gnus-article-buffer)
	   gnus-article-buffer
	   (eq (get-buffer gnus-article-buffer) (current-buffer)))
      (highlight-headers (point-min) (re-search-forward "^$" nil t) t)))

(defun x-face-xmas-replace-x-face-function-for-semi-1 (&optional arg)
  (or (boundp 'orig::mime-display-header-hook)
      (let ((w (symbol-function 'display-warning)))
	(unwind-protect
	    (progn
	      (fset 'display-warning x-face-nop)
	      (require 'mime-image))
	  (fset 'display-warning w))
	(setq orig::mime-display-header-hook
	      (and (boundp 'mime-display-header-hook)
		   (copy-sequence mime-display-header-hook)))))
  (if arg
      (setq mime-display-header-hook
	    orig::mime-display-header-hook)
    (remove-hook 'mime-display-header-hook
		 'mime-preview-x-face-function-use-highlight-headers)
    (add-hook 'mime-display-header-hook
	      'x-face-xmas-mime-highlight-headers)))

(defun x-face-xmas-replace-x-face-function-for-semi-0 (&optional arg)
  (or (boundp 'orig::mime-view-content-header-filter-hook)
      (let ((w (symbol-function 'display-warning)))
	(unwind-protect
	    (progn
	      (fset 'display-warning x-face-nop)
	      (require 'mime-image))
	  (fset 'display-warning w))
	(setq orig::mime-view-content-header-filter-hook
	      (copy-sequence mime-view-content-header-filter-hook))))
  (if arg
      (setq mime-view-content-header-filter-hook
	    orig::mime-view-content-header-filter-hook)
    ;; >= SEMI 1.1.0
    (remove-hook 'mime-view-content-header-filter-hook
		 'mime-preview-x-face-function-use-highlight-headers)
    ;; <= SEMI 1.0.2
    (remove-hook 'mime-view-content-header-filter-hook
		 'mime-preview/x-face-function-use-highlight-headers)
    (add-hook 'mime-view-content-header-filter-hook
	      'x-face-xmas-mime-highlight-headers)))

(defun x-face-xmas-replace-x-face-function-for-semi (&optional arg)
  "Replace mime-preview-x-face-function-use-highlight-headers for SEMI."
  (if (boundp 'mime-user-interface-product)
      (x-face-xmas-replace-x-face-function-for-semi-1 arg)
    (let ((version (or (and (boundp 'mime-user-interface-version)
			    mime-user-interface-version)
		       (and (boundp 'mime-module-version)
			    mime-module-version)
		       (and (boundp 'semi-version)
			    semi-version))))
      (if version
	  (if (and (>= (nth 2 version) 1)
		   (>= (nth 3 version) 7))
	      (x-face-xmas-replace-x-face-function-for-semi-1 arg)
	    (x-face-xmas-replace-x-face-function-for-semi-0 arg))
	(eval-after-load "semi-def"
	  `(x-face-xmas-replace-x-face-function-for-semi ,arg))
	(eval-after-load "mime-def"
	  `(if (or (boundp 'mime-module-version) (boundp 'semi-version))
	       (x-face-xmas-replace-x-face-function-for-semi ,arg)))))))

(defun x-face-xmas-replace-x-face-function-for-tm (&optional arg)
  "Replace mime-preview/x-face-function-use-highlight-headers for tm."
  (or (boundp 'orig::mime-viewer/content-header-filter-hook)
      (let ((w (symbol-function 'display-warning)))
	(unwind-protect
	    (progn
	      (fset 'display-warning x-face-nop)
	      (require 'tm-image))
	  (fset 'display-warning w))
	(setq orig::mime-viewer/content-header-filter-hook
	      (copy-sequence mime-viewer/content-header-filter-hook))))
  (if arg
      (setq mime-viewer/content-header-filter-hook
	    orig::mime-viewer/content-header-filter-hook)
    (remove-hook 'mime-viewer/content-header-filter-hook
		 'mime-preview/x-face-function-use-highlight-headers)
    (add-hook 'mime-viewer/content-header-filter-hook
	      'x-face-xmas-mime-highlight-headers)))

(defun x-face-xmas-xbm-file-to-glyphs (file &optional directory)
  "Generate and cache glyphs from a xbm file."
  (let* ((filename (apply 'expand-file-name
			  (if directory
			      (list (file-name-nondirectory file) directory)
			    (list file))))
	 (attributes (file-attributes filename))
	 (modtime (nth 5 attributes))
	 (size (nth 7 attributes))
	 (cache (assoc filename x-face-xmas-image-file-cache))
	 buffer ret type x y height icons glyph glyphs)
    (or (and (equal modtime (cadr cache))
	     (eq size (caddr cache))
	     (cadddr cache))
	(progn
	  (setq buffer (get-buffer-create " *x-face*"))
	  (save-excursion
	    (x-face-set-buffer-disable-undo buffer)
	    (x-face-find-file-no-conversion filename)
	    (if (setq ret (condition-case ()
			      (x-face-xbm-to-icon-strings)
			    (error nil)))
		(progn
		  (setq type (car ret)
			x (caadr ret)
			y (cdadr ret)
			icons (caddr ret))
		  (when (or (eq 'mono type) (not type))
		    (setq height (* 48 y)
			  glyph (make-glyph
				 (vector
				  'xbm :data
				  (list (* 48 x) height
					(mapconcat
					 'char-to-string
					 (x-face-ascii-to-binary-icon
					  (x-face-concat-mono-icons x y icons)
					  nil nil 'xmas)
					 "")))))
		    (set-glyph-face glyph 'x-face-xmas-x-face-face)
		    (push (cons height glyph) glyphs))
		  (when (or (eq 'rgb type) (not type))
		    (if (eq 1 y)
			(setq x (/ x 3))
		      (setq y (/ y 3)))
		    (set-buffer (x-face-icons-to-xpm
				 "noname" x y
				 (x-face-concat-rgb-icons x y icons)))
		    (setq height (* 48 y)
			  glyph (make-glyph
				 (vector 'xpm :data (buffer-string))))
		    (kill-buffer (current-buffer))
		    (push (cons height glyph) glyphs)))
	      (setq glyphs
		    (list
		     (cons (- x-face-xmas-image-xpm-filebad-height)
			   (make-glyph
			    (vector 'xpm :data
				    x-face-xmas-image-xpm-filebad)))))))
	  (setq x-face-xmas-image-file-cache
		(put-alist filename (list modtime size glyphs)
			   x-face-xmas-image-file-cache))
	  (kill-buffer buffer)
	  glyphs))))

(defun x-face-xmas-display-x-face-in-dialog-box ()
  "Display X-Face images in the dialog box."
  (and x-face-xmas-xface-p
       (memq this-command '(x-face-menu-encode x-face-menu-insert))
       (should-use-dialog-box-p)
       (save-excursion
	 (set-buffer (get-buffer "*Completions*"))
	 (let ((cw (get-buffer-window (current-buffer)))
	       (case-fold-search t)
	       (re (x-face-compressed-filename-regexp
		    "[^\n\t ]+\\.xbm\\(%s\\)?"))
	       (spc (make-glyph [xbm :data (4 1 "\x0")]))
	       (gc-cons-threshold 67108863)
	       (wh (* x-face-xmas-font-height-in-dialog-box
		      (- (cdr (assq 'height x-face-xmas-dialog-frame-alist))
			 4)))
	       (hlst (list x-face-xmas-font-height-in-dialog-box))
	       (ttl x-face-xmas-font-height-in-dialog-box)
	       (frame (selected-frame))
	       (window-min-height 1)
	       buffer-read-only
	       tmp beg end filename files glyphs good glyph extent vscroll)
	   (and (setq tmp (get-buffer " *mouse-read-file*"))
		(setq tmp (get-buffer-window tmp))
		(delete-window tmp))
	   (message (setq x-face-last-message "Creating thumbnails..."))
	   (and (setq tmp (assq 'width x-face-xmas-dialog-frame-alist))
		(modify-frame-parameters frame (list tmp)))
	   (set-specifier scrollbar-width 15 frame)
	   (set-specifier scrollbar-height 15 frame)
	   (redisplay-frame frame)
	   (goto-char (point-min))
	   (forward-line 1)
	   (delete-region (point-min) (point))
	   (while (re-search-forward "[\t ]+" nil t)
	     (replace-match "\n"))
	   (while (re-search-forward "[^\n\t ]+" nil t)
	     (setq beg (match-beginning 0)
		   end (match-end 0)
		   filename (expand-file-name (match-string 0)
					      default-directory))
	     (if (string-match re filename)
		 (progn
		   (setq files (cons filename files)
			 glyphs (x-face-xmas-xbm-file-to-glyphs filename)
			 good (> (caar glyphs) 0)
			 tmp (max (car (sort (mapcar
					      (lambda (elt) (abs (car elt)))
					      glyphs) '>))
				  x-face-xmas-font-height-in-dialog-box)
			 hlst (append hlst (list tmp))
			 ttl (+ ttl tmp)
			 glyphs (mapcar 'cdr glyphs)
			 extent nil
			 tmp nil)
		   (unless good
		     (while (setq extent (extent-at beg nil nil extent 'at))
		       (push extent tmp))
		     (mapcar (lambda (e)
			       (set-extent-properties
				e '(mouse-face
				    nil
				    list-mode-item-activate-callback nil)))
			     tmp))
		   (and (> (length glyphs) 1)
			(setq glyphs (list (car glyphs) spc (cadr glyphs))))
		   (setq glyphs (append (list spc) glyphs (list spc)))
		   (while (setq glyph (pop glyphs))
		     (and (eq 'mono-pixmap
			      (image-instance-type
			       (specifier-instance
				(setq tmp (glyph-image glyph)))))
			  (setq glyph (make-glyph tmp))
			  (set-glyph-face
			   glyph 'x-face-xmas-x-face-face-for-dialog))
		     (setq extent (make-extent beg end))
		     (set-extent-begin-glyph extent glyph)
		     (set-extent-property extent 'x-face-image t))
		   (setq vscroll 0
			 tmp ttl)
		   (while (> tmp wh)
		     (setq tmp (- tmp (nth vscroll hlst))
			   vscroll (1+ vscroll)))
		   ;; scroll up if necessary.
		   (unless (zerop vscroll)
		     (goto-char (point-min))
		     (forward-line vscroll)
		     (set-window-start cw (point))
		     (goto-char end))
		   (redisplay-frame frame))
	       (delete-region beg (1+ end))))
	   ;;-)
	   (when x-face-xmas-image-xpm-logo
	     (setq wh (cdr (assq 'height x-face-xmas-dialog-frame-alist)))
	     (goto-char (setq beg (point-max)))
	     (x-face-xmas-splash-at-point (and wh (max 0 (- wh 3))))
	     (sit-for 1)
	     (delete-region beg (point-max)))
	   ;;
	   (set-buffer-modified-p nil)
	   (setq buffer-read-only t)
	   ;; cleaning up cache.
	   (setq x-face-xmas-image-file-cache
		 (delq nil
		       (mapcar
			(lambda (elt) (and (member (car elt) files) elt))
			x-face-xmas-image-file-cache)))
	   (set-window-start cw (point-min))
	   (setq x-face-last-message nil)))))

(defun x-face-xmas-image-file-cache-precharge ()
  "A function to precharge X-Face images to the cache."
  (let ((files (x-face-directory-files
		x-face-image-file-directory t
		(x-face-compressed-filename-regexp
		 "[^\n\t ]+\\.xbm\\(%s\\)?")
		nil t))
	(num 1)
	ttl)
    (when files
      (setq ttl (length files))
      (mapcar (lambda (file)
		(message
		 (setq x-face-last-message
		       (format "Precharging X-Face images (%d/%d)..."
			       num ttl)))
		(x-face-xmas-xbm-file-to-glyphs file)
		(incf num))
	      files)
      (message "Precharging X-Face images (%d/%d)...done" ttl ttl)
      (setq x-face-last-message nil))))


;;; Menu.
(defconst x-face-menu-title "X-Face")

(defvar x-face-menu-map (make-sparse-keymap x-face-menu-title))

(defconst x-face-menu-list
  '(("Toggle Image"
     x-face-xmas-display-x-face
     (and x-face-xmas-xface-p
	  (not buffer-read-only)
	  (zerop (minibuffer-depth))))
    ("View"
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

(if (featurep 'menubar)
    (defun x-face-define-menu ()
      "Define menu for XEmacs."
      (make-local-variable 'current-menubar)
      (set-buffer-menubar current-menubar)
      (add-submenu
       nil
       (cons x-face-menu-title
	     (mapcar
	      (lambda (item) (apply 'vector item))
	      x-face-menu-list))
       "Top"))
  (defun x-face-define-menu ()
    (message "!! The feature `menubar' is not provided.")
    (sit-for 1)))


;;; Splash screen.
(defun x-face-xmas-splash-at-point (&optional height)
  (or (bolp) (insert "\n"))
  (let ((bow (point))
	bov)
    (insert-char ?\n (max 0 (/ (- (or height (window-height)) 8) 2)))
    (insert-char ?\  (max 0 (/ (- (window-width) 44) 2)))
    (set-extent-end-glyph
     (make-extent (point) (point))
     (make-glyph
      (vector 'xpm :data x-face-xmas-image-xpm-logo)))
    (insert "\n")
    (insert-char ?\  (max 0 (/ (- (window-width) (length x-face-version)) 2)))
    (setq bov (point))
    (insert x-face-version)
    (and (find-face 'bold-italic)
	 (put-text-property bov (point) 'face 'bold-italic))
    (goto-char bow)
    (set-window-start (get-buffer-window (current-buffer)) (point))
    (redisplay-frame)))

(defun x-face-xmas-splash (&optional arg)
  (interactive "P")
  (and x-face-xmas-image-xpm-logo
       (let ((frame (selected-frame))
	     config buffer)
	 (and frame
	      (unwind-protect
		  (progn
		    (setq config (current-window-configuration))
		    (switch-to-buffer
		     (setq buffer (generate-new-buffer
				   (concat (if arg "*" " *")
					   x-face-version "*"))))
		    (delete-other-windows)
		    (x-face-xmas-splash-at-point)
		    (set-buffer-modified-p nil)
		    (or arg (sleep-for 2)))
		(unless arg
		  (kill-buffer buffer)
		  (set-window-configuration config)
		  (redisplay-frame frame)))))))
(or x-face-inhibit-loadup-splash
    (eq 'stream (device-type))
    (x-face-xmas-splash))


(provide 'x-face-xmas)
(run-hooks 'x-face-xmas-load-hook)

;;; x-face-xmas.el ends here
