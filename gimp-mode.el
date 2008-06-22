;;; gimp-mode.el --- $Id: gimp-mode.el,v 1.31 2008-06-22 14:20:07 sharik Exp $
;; Copyright (C) 2008 Niels Giesen <nielsforkgiesen@gmailspooncom, but
;; please replace the kitchen utensils with a dot before hitting
;; "Send">

;; Author: Niels Giesen <nielsforkgiesen@gmailspooncom, but please
;; replace the kitchen utensils with a dot before hitting "Send">
;; Keywords: processes, multimedia, extensions, tools, gimp, scheme

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Interaction/editing mode for the GIMP; see README for full description and usage.

;; TOC:
 ;; Requirements
 ;; Structure
 ;; Faces
 ;; Globals
 ;; Customization
 ;; General purpose functions and macros
 ;; Keybindings
 ;; Set up the modes
 ;; Versioning
 ;; Evaluation
 ;; Help
 ;; Stuff pertaining to running 'n' quitting
 ;; Utility functions
 ;; Caches: saving, deleting, restoring
 ;; Tracing
 ;; Modes
 ;; Internal information retrieval
 ;; Completion
 ;; Shortcuts
 ;; Doc echoing
 ;; Source look-up
 ;; Snippets
 ;; Misc interactive commands

;;; Code:
(provide 'gimp-mode)
 ;; Requirements
(require 'cmuscheme)
(require 'outline)
(require 'cl)
(require 'eldoc)
(require 'thingatpt)
(eval-when-compile (load "gimp-init.el"))
(eval-when (compile load)
  (require 'ring)
  (require 'snippet)
  (require 'scheme-complete))
 ;; Structure
(defgroup gimp nil "Customization group for GIMP (script-fu) programming."
  :group 'shell
  :group 'scheme
  :group 'multimedia
  :group 'languages)
(defgroup gimp-faces 
  nil
  "Customization group for GIMP Mode faces"
  :group 'gimp)
 ;; Faces
(defface gimp-shy-face
  '((((class color)(background dark))
     :foreground "saddlebrown")
    (((class color)(background light))
     :foreground  "#ecf"))
"Face for uninteresting stuff."
  :group 'gimp-faces)

(defface gimp-less-shy-face
  '((((class color)(background dark))
     :foreground "darkkhaki")
    (((class color)(background light))
     :foreground  "darkslateblue"))
"Face for uninteresting stuff."
  :group 'gimp-faces)

(defface gimp-menu-face
  '((t (:foreground "#7f8c29"
        :box '(:line-width 2 :color grey15)
        :height 1.2
        :bold t
        )))
  "Face for menu items."
  :group 'gimp-faces)

(defface gimp-red-face
  '((t (:foreground "#f00"
        :bold t
        )))
  "Face for menu items."
  :group 'gimp-faces)

(defface gimp-terminal-menu-face
  '((default 
      (:inherit gimp-menu-face))
    (((class color)(background dark))
     :foreground  "#cc0")
    (((class color)(background light))
     :foreground  "#200"))
  "Face for terminal menu item."
  :group 'gimp-faces)

(defface gimp-link-face
  '((t
      (:underline t
       :foreground  "#d37511")))
  "Face for links."
  :group 'gimp-faces)

(defface gimp-visited-procedure-face
  '((t 
     (:foreground "#8c7829")))
  "Face for procedures that have been visited with GIMP Help"
  :group 'gimp-faces)

(defface gimp-level1-face
  '((default 
      :family "helv"
      :height 1.2
      :weight bold)
    (((class color)(background dark))
     :foreground  "gold3")
    (((class color)(background light))
     :foreground  "gold"))
  "Face for level2 items (== arguments)"
  :group 'gimp-faces)

(defface gimp-level2-face
  '((default 
      :weight bold)
    (((class color)(background dark))
     :foreground  "gold")
    (((class color)(background light))
     :foreground  "#200"))
  "Face for level2 items (== arguments)"
  :group 'gimp-faces)

 ;; Globals
(defconst gimp-email-regexp
  "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+.[A-Z]\\{2,4\\}\\b")
(defconst gimp-url-regexp
  "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?[-a-z0-9_=!?#$@~%&*+\\/:;.,[:word:]]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)")
(defconst gimp-interactive t
  "Provide interaction with inferior gimp process?
Leave this a non-nil-val; this might turn into a defcustom one time or
another.  Now best left at the non-nil value.")
(defvar gimp-current-procedure nil 
  "Value of queried procedure in GIMP Help buffer")
(make-variable-buffer-local 'comint-input-filter-functions)
(make-variable-buffer-local 'gimp-current-procedure)
(defvar gimp-mode-line-format
  '("["
    (:eval 
     (reduce (lambda (memo this)
	       (or (and (and memo this)(concat memo "," this)) this memo))
		(list 
		 (when gimp-complete-fuzzy-p 
		   "fuzzy")
		 (when (get 'gimp-trace 'trace-wanted) "tracing")
                 (and gimp-cl-host (concat gimp-cl-host ":"  (number-to-string gimp-cl-port))))))
    "]")
"State of various Gimp Mode options.")
(defvar gimp-cl-proc nil
  "Gimp Client Process Object")
(defvar gimp-cl-buffer-name "*Gimp-Client*"
  "Name of Gimp client buffer.")
(defvar gimp-output ""
  "Contains output from inferior gimp process.")
;; (Bases of following caches) generated on GIMP startup (by
;; emacs-interaction.scm)
(defvar gimp-dump nil 
  "Dump of pdb")
(defvar gimp-pdb-cache nil
  "Cache containing all symbols in GIMPs Procedural Database.")
(defvar gimp-registrations nil)
(defvar gimp-fonts-cache nil 
  "Cache of available fonts")
(defvar gimp-brushes-cache nil
  "Cache of available brushes")
(defvar gimp-patterns-cache nil
  "Cache of available patterns")
(defvar gimp-gradients-cache nil
  "Cache of available gradients")
(defvar gimp-palettes-cache nil
  "Cache of available palettes")
(defvar gimp-oblist-cache nil
  "Cache containing ALL symbols in TinyScheme, whether bound or not.

  The last might be subject to change.")
(defvar gimp-menu nil)
(defvar gimp-shortcuts nil
  "Commands that can be completed from inferior GIMP buffer.

Define such command with `gimp-defcommand' to be automatically included.")

(defvar gimp-help-ring ())
(setf (get 'gimp-help-ring 'gimp-help-positions)
      (make-vector 100 1))
(put 'gimp-help-ring 'index 0)

 ;; Customization
(defgroup gimp-directories nil
  "Directories where the GIMP finds its sources"
  :group 'gimp)

(defcustom gimp-src-dir (expand-file-name "~/src/gimp-2.4.2/")
  "Source directory for the GIMP.

On Debian(-derivatives), get the source for your distribution with apt-get
source gimp. You will most probably have to change this value."
  :group 'gimp-directories
  :type 'string)

(defcustom gimp-dir (expand-file-name "~/.gimp-2.4")
  "Fall-back user directory for the GIMP.

Setting this variable is only necessary for non-interactive use,
such as on windhoos.

The GIMP puts its caches here.  Retrieve it by evaluating the variable
`gimp-dir' in GIMP script-fu console."
  :group 'gimp-directories
  :type 'string)

(defcustom gimp-data-dir "/usr/share/gimp/2.0/"
  "Fall-back data dir of the GIMP.

Setting this variable is only necessary for non-interactive use,
such as on windhoos.

Value returned by evaluating the variable `gimp-dir' in the GIMP
script-fu console."
  :group 'gimp-directories
  :type 'string)

(defun gimp-src-dir ()
  (if (or
       (null gimp-src-dir)
       (not (file-exists-p gimp-src-dir)))
      (error "%s does not exist.  Check variable `gimp-src-dir'" (or gimp-src-dir "gimp-src-dir"))
    gimp-src-dir))

(defcustom gimp-docs-alist
  '(("script-fu introduction" .
     "http://www.ve3syb.ca/wiki/doku.php?id=software:sf:start")
    ("script-fu tutorial" . "http://docs.gimp.org/en/gimp-using-script-fu-tutorial.html")
    ("yahoo script-fu group" . "http://tech.groups.yahoo.com/group/script-fu/")
    ("mailing lists" . "http://www.gimp.org/mail_lists.html")
    ("developer.gimp.org" . "http://developer.gimp.org/")
    ("registry.gimp.org" . "http://registry.gimp.org/")
    ("local help" . "file:///usr/share/gimp/2.0/help/en/index.html")
    ("gimp talk" . "http://www.gimptalk.com/forum/")
    ("grokking the gimp" . "http://www.linuxtopia.org/online_books/graphics_tools/gimp_advanced_guide/index.html"))
  "Alist of gimp documentation URLs."
  :group 'gimp
  :type '(alist :key-type string :value-type string))

(defcustom gimp-program-command-line "gimp -c --batch-interpreter=plug-in-script-fu-eval -b -"
  "Arguments to give to the GIMP. 

  -v, --version                  Show version information and exit
  --license                      Show license information and exit
  --verbose                      Be more verbose
  -n, --new-instance             Start a new GIMP instance
  -a, --as-new                   Open images as new
  -i, --no-interface             Run without a user interface
  -d, --no-data                  Do not load brushes, gradients, patterns, ...
  -f, --no-fonts                 Do not load any fonts
  -s, --no-splash                Do not show a startup window
  --no-shm                       Do not use shared memory between GIMP and plugins
  --no-cpu-accel                 Do not use special CPU acceleration functions
  --session=<name>               Use an alternate sessionrc file
  -g, --gimprc=<filename>        Use an alternate user gimprc file
  --system-gimprc=<filename>     Use an alternate system gimprc file
  -b, --batch=<command>          Batch command to run (can be used multiple times)
  --batch-interpreter=<proc>     The procedure to process batch commands with
  -c, --console-messages         Send messages to console instead of using a dialog
  --pdb-compat-mode=<mode>       PDB compatibility mode (off|on|warn)
  --stack-trace-mode=<mode>      Debug in case of a crash (never|query|always)
  --debug-handlers               Enable non-fatal debugging signal handlers
  --g-fatal-warnings             Make all warnings fatal
  --dump-gimprc                  Output a gimprc file with default settings
  --display=DISPLAY              X display to use"
  :group 'gimp
  :type 'string)

(defcustom gimp-inhibit-start-up-message nil
  "Inhibit start-up message in Inferior Gimp Mode."
  :group 'gimp
  :type 'boolean)

(defcustom gimp-just-one-space nil
  "When issuing `gimp-space', act as `just-one-space'?"
  :group 'gimp
  :type 'boolean)

(defcustom gimp-insert-quotes-for-strings-p t
  "Insert quotes when the current argument is a string?

With its value set to nil, gimp-mode will still offer contextual
completion (when possible) when point is in a string."
  :group 'gimp
  :type 'boolean)

 ;; General purpose functions and macros
(defmacro gimp-defcommand (name arglist docstring &rest body)
"Define NAME as a function.
Put its name without the gimp-prefix in variable `gimp-shortcuts'.

The definition is (lambda ARGLIST DOCSTRING BODY...).
If no `interactive' form is used in BODY, an error is signalled."
  (declare (indent defun))
  (when (not (stringp docstring))
    (error "Error defining %S: docstring seems to be missing" name))
  `(progn
     (eval-when (load eval) (add-to-list 'gimp-shortcuts ,(replace-regexp-in-string
				    "^gimp-" "" (symbol-name name))))
     (defun ,name (,@arglist)
       ,docstring
       ,@body)
     (when (not (commandp ',name))
       (error "Error: function %S defined, but not interactive" ',name))))

(defmacro gimp-hash-to-list (hash-table)
  (let ((nl (gensym)))
    `(let (,nl)
       (maphash (lambda (k v)
                  (push (list k v) ,nl)) ,hash-table)
       ,nl)))

(defmacro gimp-list-to-hash (list)
  (let ((ht (gensym)))
    `(let ((,ht (make-hash-table)))
       (mapc (lambda (item)
               (puthash (car item) (cadr item) ,ht)) ,list)
       ,ht)))

(defmacro gimp-without-string (&rest body)
  `(save-excursion
     (if (gimp-in-string-p)
         (gimp-up-string))
     ,@body))

(defun gimp-string-match (re str &optional num)
  (when (string-match re str)
    (if num (match-string num str)
      (let (result)
	(dotimes (v (/ (length (match-data)) 2) (reverse result))
	  (push (match-string v str) result))))))

(defun gimp-test-recursively (fun list max)
  "Test FUN on subsequent items in LIST, for MAX recursions."
  (cond 
   ((< (decf max) 0))
   ((endp (cdr list)) t)
   ((apply fun
	   (car list)
	   (list (cadr list)))
    (gimp-test-recursively fun (cdr list) max))
   (t nil)))

;; Emacs 22 compatibility for rings
(eval-when (compile load)
  (when (not (fboundp 'ring-member))
    (defun ring-member (ring item)
      "Return index of ITEM if on RING, else nil.
Comparison is done via `equal'.  The index is 0-based."
      (catch 'found
	(dotimes (ind (ring-length ring) nil)
	  (when (equal item (ring-ref ring ind))
	    (throw 'found ind)))))))
(eval-when (compile load)
  (when (not (fboundp 'ring-next))
    (defun ring-next (ring item)
      "Return the next item in the RING, after ITEM.
Raise error if ITEM is not in the RING."
      (let ((curr-index (ring-member ring item)))
	(unless curr-index (error "Item is not in the ring: `%s'" item))
	(ring-ref ring (ring-plus1 curr-index (ring-length ring)))))))

;; Adapted from elmo-util.el
(defun gimp-uniq-list! (lst)
  "Destructively uniquify elements of LST. Quite fast."
  (let ((tmp lst))
    (while tmp
      (setq tmp
	    (setcdr tmp
		    (and (cdr tmp)
			 (delete
			  (car tmp)
			  (cdr tmp)))))))
  lst)

(defun gimp-regular-files-in-dir (dir)
  "List all regular files in DIR, "
  (loop for f in (directory-files dir t)
        when (file-regular-p f)
        collect f))

(defun gimp-de-underscore (list-or-item)
  "Return LIST-OR-ITEM with all underscores recursively eradicated."
  (cond ((listp list-or-item)
         (loop for i in list-or-item
               when (not (eq '_ i))
               collect (gimp-de-underscore i)))
        (t list-or-item)))

(defun gimp-buffer-window (buffer)
  (get-window-with-predicate
   (lambda (w)
     (eq (window-buffer w)
	 (get-buffer buffer)))))
 ;; Keybindings
(defvar inferior-gimp-mode-map
  (let ((m (copy-keymap inferior-scheme-mode-map)))
    (define-key m "\t" 'gimp-indent-and-complete)
    (define-key m " " 'gimp-space)
    (define-key m "\C-c," 'gimp-describe-this-arg)
    (define-key m "\C-c." 'gimp-echo)
    (define-key m "\C-cf" 'gimp-describe-procedure)
    (define-key m "\C-ca" 'gimp-apropos)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-code-search)
    (define-key m "\C-cm" 'gimp-menu)
    (define-key m "\C-m" 'gimp-send-input)
    (define-key m "\M-\\" 'comint-dynamic-complete-filename)
    (define-key m "\C-cr" 'gimp-toggle-fuzzy-completion)
    (define-key m [mouse-1] 'gimp-insert-input)
    (define-key m [mouse-2] 'gimp-insert-input)
    m))

(defvar gimp-mode-map
  (let ((m (copy-keymap scheme-mode-map)))
    (define-key m "\t" 'gimp-indent-and-complete)
    (define-key m " " 'gimp-space)
    (define-key m "\C-x\C-e" 'gimp-send-last-sexp)
    (define-key m "\C-c," 'gimp-describe-this-arg)
    (define-key m "\C-c." 'gimp-echo)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-cf" 'gimp-describe-procedure)
    (define-key m "\C-ca" 'gimp-apropos)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-code-search)
    (define-key m "\C-cm" 'gimp-menu)
    (define-key m "\C-ci" 'run-gimp)
    (define-key m "\C-cr" 'gimp-toggle-fuzzy-completion)
    (define-key m "\M-\\" 'comint-dynamic-complete-filename)
    m))

(defvar gimp-help-mode-map
  (let ((m (copy-keymap outline-mode-map)))
    (define-key m "\C-m" 'gimp-help-enter)
    (define-key m "\t" 'gimp-next-field)
    (define-key m [(backtab)]
      (lambda (n)
	"Hop fields backwards."
        (interactive "p")
        (gimp-next-field (- n))))
    (define-key m " " 'gimp-space)
    (define-key m "," 'gimp-echo-at-point)
    (define-key m "f" 'gimp-help-forward)
    (define-key m "b" 'gimp-help-back)
    (define-key m "F" 'gimp-describe-procedure)
    (define-key m "a" 'gimp-apropos)
    (define-key m "q" 'bury-buffer)
    (define-key m "p" 'previous-line)
    (define-key m "n" 'next-line)
    (define-key m "i" 'run-gimp)
    (define-key m "d" 'gimp-documentation)
    (define-key m "s" 'gimp-code-search)
    (define-key m "m" 'gimp-menu)
    (define-key m "i" 'gimp-insert-sexp-at-repl)
    (define-key m "t" 'outline-toggle-children)
    (define-key m "S" 'gimp-selector)
    (define-key m "R" 'gimp-help-refresh)
    (define-key m "\C-cf" 'gimp-describe-procedure)
    (define-key m "\C-ca" 'gimp-apropos)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-code-search)
    (define-key m "\C-cm" 'gimp-menu)
    m))

(define-key gimp-help-mode-map
  [(down-mouse-1)]
  (lambda ()
    "Put point where mouse is, and do what ENTER does at that spot."
    (interactive)
    (let ((event (read-event)))
      (mouse-set-point event)
      (gimp-help-enter))))

(defun gimp-help-enter (&optional event)
  "Dispatch enter key or mouse click in GIMP Help buffer.
Optional argument EVENT is a mouse event."
  (interactive)
  (cond ((or (eq (field-at-pos (point)) 'submenu)
             (eq (field-at-pos (1+ (point))) 'submenu))
         (gimp-menu
          (gimp-submenu-at-point (point))))
        ((or (eq (field-at-pos (point)) 'procedure)
             (eq (field-at-pos (1+ (point))) 'procedure))
	 (let ((char (read-char "Insert at REPL or search source code [is]: ")))
	   (case char
	     (?i (gimp-insert-sexp-at-repl))
	     (?s (gimp-code-search (symbol-name (gimp-procedure-at-point)))))))
        ((gimp-procedure-at-point)
         (gimp-describe-procedure (gimp-procedure-at-point t)))
        ((or (eq (field-at-pos (point)) 'email)
             (eq (field-at-pos (1+ (point))) 'email))
         (mail nil (field-string-no-properties (1+ (point)))
               (symbol-name gimp-current-procedure)))
        ((or (eq (field-at-pos (point)) 'url)
             (eq (field-at-pos (1+ (point))) 'url))
         (browse-url (field-string-no-properties (1+ (point)))))
        ((or (eq (field-at-pos (point)) 'function)
             (eq (field-at-pos (1+ (point))) 'function))
         (let ((str (field-string-no-properties (1+ (point)))))
           (gimp-describe-procedure (substring str 1 (1- (length str))))))
        ((or (eq (field-at-pos (point)) 'forward)
             (eq (field-at-pos (1+ (point))) 'forward))
         (gimp-help-forward))
        ((or (eq (field-at-pos (point)) 'back)
             (eq (field-at-pos (1+ (point))) 'back))
         (gimp-help-back))

        (t (error "Nothing to do at point"))))

(defun gimp-space (n)
  "Dispatch space to DWIM actions:

- in help (apropos) mode: echo documentation and move to next line
- in other modes:
 - when in a string: insert N spaces
 - otherwise: echo documentation"
  (interactive "p")
  (cond  ((eq major-mode 'gimp-help-mode)
          (if (gimp-describe-procedure-at-point)
              (unless (= (point-at-eol) (point-max))
                (call-interactively 'next-line))
            (scroll-up n)))
	 (t
          (if gimp-just-one-space
	      (just-one-space n)
	    (self-insert-command n))
;	  (indent-for-tab-command)
          (gimp-echo))))
 ;; Set up the modes
(define-derived-mode gimp-mode scheme-mode "GIMP mode" 
  "Mode for editing script-fu and interacting with an inferior gimp process."
  (use-local-map gimp-mode-map)
  (abbrev-mode 1)
  (setq local-abbrev-table gimp-mode-abbrev-table)
  (add-to-list 'mode-line-process gimp-mode-line-format t)
  (setq indent-line-function 'lisp-indent-line)
  (if (null gimp-oblist-cache)
      (gimp-restore-caches))) 

(define-derived-mode inferior-gimp-mode inferior-scheme-mode
  "Inferior GIMP"
  "Mode for interaction with inferior gimp process."
  (use-local-map inferior-gimp-mode-map)
  (setq comint-input-filter-functions 
	'(gimp-add-define-to-oblist))
  (add-to-list 'mode-line-process gimp-mode-line-format t))

 ;; Versioning
(gimp-defcommand gimp-gimp-mode-version ()
  "Version of this mode."
  (interactive)
  (destructuring-bind (version major minor) 
      (gimp-string-match "\\([0-9]+\\)\.\\([0-9]+\\)"
                         "$Id: gimp-mode.el,v 1.31 2008-06-22 14:20:07 sharik Exp $" )
      (if (interactive-p) 
          (prog1 nil 
            (message "GIMP mode version: %s.%s" major minor))
        (format "%s.%s" major minor))))

(defalias 'gimp-mode-version 'gimp-gimp-mode-version)

(gimp-defcommand gimp-gimp-version ()
  "Version of the GIMP."
  (interactive)
  (destructuring-bind (version major minor rev)
      (gimp-string-match
       "\\([[:digit:]]\\)\.\\([[:digit:]]\\)\.\\([[:digit:]]\\)"
       (gimp-eval "(car (gimp-version))"))
    (when (interactive-p)
      (message "GIMP version: %s" version))
    (list version major minor rev)))
 ;; Evaluation
(defmacro in-gimp (body)
  "Evaluate fu sexps without having to quote them. Syntactic sugar.

Argument BODY is a SCHEME sexp. Caveats:

1. Inclusion of literal Scheme vectors is impossible. This is due to read
syntax of Emacs lisp and the use of [] in scheme as if they were
parentheses. Be sure to use portable (vector elem1 elem2...) if you want to
make a vector in SCHEME with `in-gimp'.

2. As body must be a single sexp, use (begin ...) in your scheme code.

3. Possibly others. Use it lightly."
  `(gimp-eval
    (format "%S" (backquote ,body))))

;; Core inferior interaction functions
(defun gimp-proc ()
  (or (scheme-get-process)
      gimp-cl-proc))

(defun gimp-filter (proc string)
  "Filter for inferior gimp-process."
  (setq gimp-output
        (replace-regexp-in-string "Eval"  "" (concat gimp-output string))))

(defun gimp-set-comint-filter ()
  (set-process-filter
   (gimp-proc)
   'gimp-comint-filter))

(defun gimp-comint-filter (proc string)
  (gimp-filter proc string)
  
  (comint-output-filter
   proc
   (let ((string (replace-regexp-in-string
		  "\n> \nEval: (tracing 0)\nEval: tracing\nEval: 0\nApply to: (0)1"
		  "" string)))
     (setq string
	   (replace-regexp-in-string "^\\(?:Apply to\\|Eval\\|Gives\\): .*\n"
				     (lambda (m)
				       (propertize m 'font-lock-face
						   (case
						       (aref m 0)
						     (?E 'gimp-shy-face)
						     (?A 'gimp-less-shy-face)
						     (?G 'gimp-visited-procedure-face)))) string))
     (replace-regexp-in-string "[]()[]"
			       (lambda (m)
				 (propertize m
					     'font-lock-face
					     'gimp-red-face)) string))))

(defun scheme-send-string (string &optional newline)
  "Send STRING to the scheme process.
When optional argument NEWLINE is non-nil, append a newline char."
  (gimp-add-define-to-oblist string)
  (comint-send-string 
   (gimp-proc)
   (concat string (if newline "\n" ""))))

(defun gimp-send-last-sexp (&optional insert)
  "Send the previous sexp to the inferior Scheme process."
  (interactive "P")
  (let ((result
	 (gimp-eval-to-string
	  (buffer-substring-no-properties
	   (save-excursion (backward-sexp) (point)) (point)))))
    (message "%s" result)
    (when (and insert (not (string-equal result "")))
      (insert (substring result 0 -1)))))

(defun gimp-eval (string)
  "Eval STRING, and return it read, somewhat, though not fully, elispified.

Best is to craft STRING so that script-fu returns something universal to the
Lisp world."
  (let ((output (gimp-eval-to-string string)))
    (if (string-match "^#" output)	;unreadable by the lisp reader
        (if (string= "#f\n> " gimp-output) ;gimp returned #f
            nil
          (read (substring output 1)))	;so strip
      (read output))))

(defun gimp-eval-to-string (string &optional discard)
  (cond
   ((gimp-interactive-p)
    (setq gimp-output "")
    (set-process-filter (gimp-proc) 'gimp-filter)
    (scheme-send-string string t)
    (unless discard
      (while (not (string-match "^> $" gimp-output)) ;prompt has not yet returned
        (sit-for .01))				   ;keep polling
      (substring gimp-output 0 -2)))
   ((eq (process-status gimp-cl-proc)
        'open) (gimp-cl-eval-to-string string discard))
   (t "nil")))                 ;strip prompt

(defun gimp-insert-input (event)
  "Insert input field at point after prompt.
Argument EVENT is a mouse-event."
  (interactive "e")
  (mouse-set-point event)
  (let ((pos (posn-point (event-end event)))
        input)
    (with-selected-window (posn-window (event-end event))
      (when (eq (field-at-pos pos) 'input)
        (setq input (field-string-no-properties pos))))
    (comint-insert-input event)
    (when (member input gimp-shortcuts)
      (save-excursion
        (comint-bol-or-process-mark)
        (insert ?,)))))

(defun gimp-add-define-to-oblist (str)
  "Put vars, functions and macros defined by STR in the oblist."
  (set-text-properties 0 (length str) nil str)
  (let* ((var-or-fun (gimp-string-match
                      "[[:space:]]*(define\\(-macro\\)?[[:space:]]+(?\\([[:word:]-?!><]+\\)" str 2)))
    (if (and var-or-fun (not (member var-or-fun gimp-oblist-cache)))
	(push var-or-fun gimp-oblist-cache))))

(defun dotted-to-list (dl)
  "'Undot' DL.

Turn DL (of the form (\"a\" \"b\" . \"c\")) into a list of the form (\"a\"
\"b\" \". c\"))."
  (cond ((atom (cdr dl))
         (cons (car dl)
               (if (endp (cdr dl))
                   nil
                 (list (format ". %s" (cdr dl))))))
        (t (cons (car dl)
                 (dotted-to-list (cdr dl))))))

(defun gimp-insert-and-send-string (string)
  "In *GIMP* buffer, set the current string to STRING."
  (save-window-excursion
    (switch-to-buffer (process-buffer (gimp-proc)))
    (unless (null comint-accum-marker)
      (goto-char (point-max))
      ;; First delete any old unsent string at the end
      (delete-region
       (or (marker-position comint-accum-marker)
	   (process-mark (get-buffer-process (current-buffer))))
       (point))
      ;; Insert the string 
      (insert string)
      ;; Send it
      (gimp-send-input))))

 ;; Help
(defvar gimp-help-visited-pages nil
  "List of visited pages.")
(defmacro gimp-help-wrapper (form &rest body)
  `(progn
     (when (and 
            ,form                       
            (not (member ,form gimp-help-ring))) ;new branche
       (setq gimp-help-ring 
             (member (nth (get 'gimp-help-ring 'index) gimp-help-ring)
                     gimp-help-ring)) ;set help ring to history until now
       (push ,form gimp-help-ring)
       (put 'gimp-help-ring 'index 0)
       (setf 
        (aref (get 'gimp-help-ring 'gimp-help-positions)
              (1- (length gimp-help-ring)))
        (point)))
     (let ((window (gimp-buffer-window "*GIMP Help*")))
       (if window
           (select-window window)
         (when (= (length (window-list)) 1)
           (split-window-vertically (round (* .60 (window-height)))))
         (other-window 1)
         (switch-to-buffer
          "*GIMP Help*")))
     (gimp-help-mode)
     (let (buffer-read-only)
       (erase-buffer)
       ,@body
       (save-excursion 
         (goto-char (point-max))
         (insert "\n\n" (if (< (get 'gimp-help-ring 'index)
			       (1- (length gimp-help-ring)))
			    (propertize "[back]" 
					'mouse-face 'highlight
					'field 'back
					'help-echo "Back in Help History"
					'font-lock-face 'gimp-link-face)
			  (propertize "[back]" 
				      'font-lock-face 'gimp-shy-face))
                 " "
                 (if (> (get 'gimp-help-ring 'index) 0)
                     (propertize "[forward]" 
                                 'mouse-face 'highlight
                                 'field 'forward
                                 'help-echo "Forward in Help History"
				 'font-lock-face 'gimp-link-face)
		   (propertize "[forward]" 
			       'font-lock-face 'gimp-shy-face))
                 " ")))))


(gimp-defcommand gimp-help ()
  "Generic GIMP help."
  (interactive)
  (let ((window (get-window-with-predicate
		 (lambda (w)
		   (eq (window-buffer w)
		       (get-buffer "*GIMP Help*"))))))
    (if window
        (select-window window)
      (when (= (length (window-list)) 1)
	(split-window-vertically (round (* .6 (window-height)))))
      (other-window 1)
      (switch-to-buffer
       "*GIMP Help*")
      (gimp-help-mode)
      (if (= (point-min)
             (point-max))
          (let (buffer-read-only)
            (insert
	     (propertize "GIMP Help\n"
			 'font-lock-face 'gimp-level1-face)
	     (propertize (concat (make-string (window-width) ?=) "\n")
			 'font-lock-face 'gimp-shy-face)
	     (propertize "Keys:\n\n"
			 'font-lock-face 'gimp-level2-face)
	     "a : gimp-apropos,\nF : function query\nm : select a procedure via the menu\ns : search code\nS : gimp-selector\n\n"
                    "For general help on GIMP Mode, please consult the README file.")
            (goto-char (point-min)))))))

(defun gimp-next-field (num)
  "Hop clickable fields in GIMP Help buffer."
  (interactive "p")
  (dotimes (n (abs num))
    (let ((next-field
           (if (< num 0)
               (let ((fb (field-beginning (point) t)))
                 (if (<= fb (point-min))
                     fb
                   (field-beginning (1- fb) t)))
             (1+ (field-end (point) t)))))
      (if (or (>= next-field (point-max))
              (>= (field-end next-field) (point-max))
              (<= next-field (point-min)))
          (error "No next field")
        (goto-char next-field))
      (when (member (field-at-pos (point))
		    '(email url procedure))
        (backward-char 1))
      (when (null (field-at-pos (1+ (point))))
        (gimp-next-field 1)))))

(gimp-defcommand gimp-menu (&optional submenu)
  "Navigate through GIMP menu structure.
Optional argument SUBMENU defines the default content of the minibuffer."
  (interactive)
  (let* ((entry (completing-read "Select by menu entry: "
                                 (mapcar 'cadr gimp-menu) nil t (or submenu "<")))
         (plug-in (car (rassoc* entry gimp-menu :key 'car :test 'string=))))
    (gimp-describe-procedure plug-in)))

(defun gimp-submenu-at-point (pos)
  "Return submenu at POS."
  (save-excursion 
    (goto-char pos)
    (do ((string (field-string-no-properties (1+ (point)))
                 (concat (field-string-no-properties) "/" string)))
        ((progn (goto-char (1- (field-beginning)))
                (not (eq (field-at-pos (point)) 'submenu))) 
         (concat (replace-regexp-in-string "/*$" "" string) "/")))))

(defun gimp-insert-sexp-at-repl ()
  "Insert sexp at point at the GIMP REPL.

Deletes any previous stuff at that REPL"
  (interactive)
  (let ((sexp (thing-at-point 'sexp)))
    (when sexp
      (switch-to-buffer-other-window (gimp-buffer))
      (goto-char (point-max))
      ;; First delete any old unsent input at the end
      (delete-region
       (or (marker-position comint-accum-marker)
	   (process-mark (get-buffer-process (current-buffer))))
       (point))
      (set-text-properties 0 (length sexp) () sexp)
      ;; Insert the input at point
      (insert sexp))))

(defun gimp-help-refresh ()
  (eval (nth 0 gimp-help-ring)))
 ;; Stuff pertaining to running 'n' quitting

;;;###autoload
(defun run-gimp ()
  "Start the GIMP and its REPL."
  (interactive)
  (if (buffer-live-p (gimp-buffer))
      (if (gimp-buffer-window (gimp-buffer))
	  (select-window (gimp-buffer-window (gimp-buffer)))
	(switch-to-buffer-other-window (gimp-buffer)))
    (run-scheme gimp-program-command-line) 
    (set-process-sentinel 
     (gimp-proc)
     (lambda (p s)
       (if (string-match "finished" s)
           (error "Probably the GIMP is already running.

Either close that instance, add the -e switch to `gimp-program-command-line',
or run command `gimp-cl-connect'.")
         (message s))))
    (save-window-excursion 
      (switch-to-buffer (gimp-buffer))
      (setq buffer-read-only t))
    (set-process-filter
     (gimp-proc)
     'gimp-first-run-action)))

(defun gimp-first-run-action (proc string)
  (set-process-sentinel 
   (gimp-proc)
   (lambda (p s)
     (ignore)))
  (switch-to-buffer (gimp-buffer))
  (let (buffer-read-only)		;make the buffer capable
                                        ;of receiving user input etc.
    (insert string)
    (unless (string= (buffer-name)
                     "*GIMP*")
      (rename-buffer "*GIMP*"))
    (setq scheme-buffer (current-buffer))
    (inferior-gimp-mode)
    (gimp-set-comint-filter)   ;set comint filter for subsequent input
    (gimp-progress (concat (current-message) "...waiting to finish...")
		     (lambda ()
		       (not (string-match "> $"  gimp-output)))
		     "done!")
    (gimp-restore-caches)
    (gimp-restore-input-ring)
    (unless gimp-inhibit-start-up-message
      (gimp-shortcuts t))
    (message "%s The GIMP is loaded. Have FU." (or (current-message) ""))) ;set comint filter for subsequent input)
  (setq buffer-read-only nil))

(defun gimp-progress (message test &optional end-text)
  "Show MESSAGE with rotating thing at end while TEST yields a non-nil value.
Optional argument END-TEXT specifies the text appended to the message when TEST fails."
  (let ((r (make-ring 4)))
    (mapc (lambda (i)
            (ring-insert r i))
          '(45 47 124 92))
    (message "%s" (concat message "/"))
    (while (funcall test)
      (let* ((mess (or (current-message) (concat message "/")))
             (last-char (string-to-char
                         (substring mess (1- (length mess))
                                    (length mess)))))
        (message "%s%c" (substring mess 0 (1- (length mess))) (ring-next r last-char))
        (sit-for .15)))
    (if end-text (message "%s%s" (current-message) end-text))))

(defalias 'gimp-start 'run-gimp 
  "Alias so people gimp-TABbing can find `run-gimp'")

(gimp-defcommand gimp-quit ()
  "Quit gimp session."
  (interactive)
  (gimp-save-input-ring)
  (gimp-eval-to-string "(gimp-quit 0)" t)
  (kill-buffer (gimp-buffer))
  (message "GIMP process ended."))

 ;; Utility functions
(defun gimp-buffer ()
  (let ((proc (gimp-proc)))
    (when proc
      (process-buffer (gimp-proc)))))

(defun gimp-dir ()
  (if (not (gimp-interactive-p))
      gimp-dir
      (gimp-eval "gimp-dir")))

(defun gimp-data-dir ()
  (if (gimp-interactive-p)
      (in-gimp gimp-data-dir)
    gimp-data-dir))

(defun gimp-all-scm-files ()
  "Produce a list of all script-fu files."
  (reduce
   (lambda (memo dir)
     (append memo (gimp-regular-files-in-dir dir)))
   (list (format "%s/scripts" (gimp-data-dir))
         (format "%s/scripts" (gimp-dir)))
   :initial-value nil))

(defun gimp-up-string ()
  "Move point to a place presumable not in a string."
  (when (re-search-backward "[^\\]\"" nil t)
    (forward-char 1)))

(defun gimp-beginning-of-sexp ()
  (let ((parse-sexp-ignore-comments t))
    (condition-case err
        (while (progn
                 (forward-sexp -1)
                 (or (= (char-before) ?\")
                     (> (point) (point-min)))))
      (error nil))))

(defun gimp-interactive-p ()
  "Is gimp being run as a subprocess?"
  (and
   gimp-interactive
   scheme-buffer
   (get-buffer scheme-buffer)
   (comint-check-proc scheme-buffer)))

(defun gimp-procedure-at-point (&optional as-string)
  (let  ((sym (when (gethash (symbol-at-point) gimp-dump)
                (symbol-at-point))))
    (when sym
      (apply 
       (if as-string
           'symbol-name
         'identity)
       (list sym)))))

(defun gimp-current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (read (current-word)))))

(defun gimp-fnsym-in-current-sexp ()
  "Return function symbol in current sexp."
  (let ((p (point)))
    (gimp-without-string
     (when  (not (looking-back ",[[:alnum:]- ]+"))
       (with-syntax-table scheme-mode-syntax-table
	 (while 
	     (and (let ((m (process-mark (gimp-proc))))
		    (if 
			(eq (marker-buffer m)
			    (current-buffer))
			(not (= (point)
				(marker-position m)))
		      t))		;no rules for other types of buffers.
		  (or 
		   (when (eq (char-syntax (char-before)) ?\")
		     (backward-sexp 1)
		     t)
		   (memq (char-syntax (char-before)) '(?w ?_ 32 ?- ?\" ?> ?'))
		   (when (memq (char-syntax (char-before)) '(?\)))
		     (backward-sexp 1)
		     t)))
	   (forward-char -1)))
       (prog1
	   ;; Don't do anything if current word is inside a string.
	   (if (= (or (char-after (1- (point))) 0) ?\")
	       nil
	     (gimp-current-symbol))
	 (goto-char p))))))

(defun gimp-position ()
  "Return position of point in current lambda form."
  (if (bolp)		 ;correct, but does not intercept all possible
                                        ;0-positions, of course
      0
    (gimp-without-string
     (let ((count 0))
       (if (not (looking-at "[ \n\t)(\"']+"))
	   (forward-sexp 1))
       (if (looking-back "[ \n\t)(]+")
	   (incf count))
       (while (not (or (looking-back "( *")
		       (looking-back comint-prompt-regexp)))
	 (backward-sexp 1)
	 (incf count))
       (decf count)))))

;; adapted from scheme-in-string-p in scheme-complete.el
(defun gimp-in-string-p ()
  "Is (point) in a string?"
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (let ((parses (parse-partial-sexp (point) orig)))
        (nth 3 parses)))))

(defun gimp-in-comment-p ()
  "Is (point) in a string?"
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (let ((parses (parse-partial-sexp (point) orig)))
        (nth 4 parses)))))
 ;; Caches: saving, deleting, restoring
(defun gimp-save-cache (cache)
  (with-temp-file (format "%s/emacs-%s" (gimp-dir) cache)
    (let ((cache (symbol-value cache)))
      (insert (format "%S" (if (hash-table-p cache)
                               (gimp-hash-to-list cache)
                             cache))))))

(defun gimp-restore-cache (cache &optional to-hash prefix)
  "Restore cache from disk.
Argument CACHE is the cache to restore.
Optional argument TO-HASH means convert the list to a hash.
Optional argument PREFIX specifies a prefix for the filename."
  (let* ((prefix (or prefix "/emacs/emacs-"))
         (file (format "%s/%s%s" (gimp-dir) prefix cache)))
    (with-temp-buffer
      (when (file-exists-p file)
        (find-file-literally file)
					;        (goto-char (point-min))
        (set cache (prog1 (read (buffer-substring (point-min) (point-max)))
                     (kill-buffer nil)))
        (when to-hash (set cache (gimp-list-to-hash (symbol-value cache))))))
    (symbol-value cache)))

(defun gimp-restore-caches ()
  "Restore caches from disk.
Normally this function needn't be run interactively, lest a cache has been
screwed up.  It is wise then to preceed it with a call to
`gimp-delete-caches'."
  (interactive)
  (when (or (not (file-exists-p (concat (gimp-dir) "/emacs/")))
	    (not (file-directory-p (concat (gimp-dir) "/emacs/"))))
      (make-directory (concat (gimp-dir) "/emacs/"))
      (gimp-dump-for-emacs))
  (message "Reading in caches... ")
  (mapc 'gimp-restore-cache
        '(gimp-menu
          gimp-fonts-cache
	  gimp-brushes-cache
	  gimp-patterns-cache
	  gimp-gradients-cache
	  gimp-palettes-cache))
  (setq gimp-oblist-cache
        (mapcar 'symbol-name
                (gimp-uniq-list!
                 (gimp-restore-cache
                  'gimp-oblist-cache))))
  (gimp-read-dump)
  (condition-case err
      (gimp-scrape-fu-registrations)
    (error (gimp-report-bug err)))
  (message "%s%s" (current-message) "Done!"))

(defun gimp-read-dump ()
  (let ((file (concat (gimp-dir) "/dump.db"))
        (ht (make-hash-table :test 'eql)))
    (with-temp-buffer
      (insert-file-contents file)
      (mapc (lambda (i)
		(puthash (intern (cadr i))
			 (cddr i)
			 ht))
	      (read (concat 
		     "("
		     (buffer-substring-no-properties
		      (point-min) (point-max))
		     ")")))
      (kill-buffer nil))
    (setq gimp-dump ht)
    (setq gimp-pdb-cache (let (list)
                           (maphash (lambda (k v)
                                      (push (symbol-name k) list)) 
                                    gimp-dump)
                           list))))
;; Input ring
(defun gimp-save-input-ring ()
  "Save the input ring for subsequent sessions."
  (let ((r
         (with-current-buffer (gimp-buffer) comint-input-ring)))
    (with-temp-file (format "%s/emacs/emacs-%s" (gimp-dir) "comint-input-ring")
      (insert (format "%S" r)))))

(defun gimp-restore-input-ring ()
  (with-current-buffer (gimp-buffer)
    (gimp-restore-cache 'comint-input-ring)
    (if (null comint-input-ring)
        (set 'comint-input-ring (make-ring 65)))))

(defun gimp-scrape-fu-registrations ()
  (setq gimp-registrations
        (let (result)
          (mapc
           (lambda (f)
             (with-temp-buffer 
               (insert-file-contents-literally f)
               (while (re-search-forward
                       "([[:space:]]*script-fu-register[[:space:]]+\"[[:alnum:]-]+\""
                       nil t)
                 (with-syntax-table scheme-mode-syntax-table
		   (up-list -1)
		   (push (gimp-de-underscore 
                          (cdr (sexp-at-point))) result)
		   (forward-sexp 1)))))
           (gimp-all-scm-files))
          result)))

(defun gimp-registrations ()
  gimp-registrations)

(defun gimp-arg-from-scraped-registration (pos procstr)
  (let* ((defaults  (loop for specs in 
                          (cdddr
                           (cddddr
                            (assoc procstr
                                   (gimp-registrations))))
                         collect specs))
         (options (nth (1- (* 3 (1+ pos))) defaults))
         (type (nth (* 3 pos) defaults)))
    (cond 
     ((listp options)
      (case type
        (SF-ADJUSTMENT
         (mapcar* 'list
                        '(init lower upper step_inc page_inc digits type)
                        (cadr options)))
        (SF-OPTION
         (cadr options))
        (t (cadr options))))
     (t (list options)))))

(defun gimp-dump-for-emacs ()
  "Dump stuff for emacs."
  (interactive)
  (let ((command "(car (script-fu-dump-for-emacs TRUE TRUE TRUE TRUE TRUE TRUE TRUE))"))
    (if (eq this-command 'gimp-send-input)
        command
      (message "Dumping stuff...")
      (gimp-eval command))))

(defun gimp-refresh-all ()
  "Update both the GIMP's and GIMP Mode's knowledge on all scripts and symbols."
  (interactive)
  (gimp-refresh-scripts)
  (gimp-dump-for-emacs)
  (gimp-restore-caches)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer "*GIMP Help*")
     (gimp-help-refresh))))
 ;; Tracing
(gimp-defcommand gimp-trace ()
  "Start tracing FU.

Only for REPL input."
  (interactive)
  (when (gimp-cl-p)
    (error "Tracing in client mode unimplemented"))
  (message "Tracing turned on")
  (put 'gimp-trace 'trace-wanted t))

(gimp-defcommand gimp-untrace ()
  "Stop tracing FU.

Only for REPL input."
  (interactive)
  (when (gimp-cl-p)
    (error "Tracing in client mode unimplemented"))
  (gimp-eval "(tracing 0)\n")
  (message "Tracing turned off")
  (put 'gimp-trace 'trace-wanted nil))
 ;; Modes
(define-derived-mode gimp-help-mode outline-mode "GIMP Help"
  "Help mode for the GIMP."
  (use-local-map gimp-help-mode-map)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (if (null gimp-oblist-cache)
      (gimp-restore-caches)))

(gimp-defcommand gimp-documentation ()
  "Shortcut to (online) documentation.

See variable `gimp-docs-alist'"
  (interactive)
  (let ((doc (completing-read "Documentation: " gimp-docs-alist nil t)))
    (browse-url (cdr (assoc doc gimp-docs-alist)))))

;; (defun gimp-apropos-list (input)
;;   (loop for i in (sort (mapcar (lambda (l)
;;                                  (symbol-name (car l)))
;;                                (gimp-hash-to-list gimp-dump)) 'string<) 
;;         when (string-match input i) 
;;         collect i))

;; (defun gimp-apropos-list-by-key (key value)
;;   (let* ((lookup '(short long author copyright date type))
;; 	 (pos (- 6 (length (member key lookup))))
;; 	 result)
;;     (maphash (lambda (k v)
;; 	       (if (string-match value (nth pos v))
;; 		   (push (symbol-name k) result)))
;; 	     gimp-dump)
;;     (sort result 'string<)))

(defun gimp-real-apropos (value)
  (let (result)
    (maphash (lambda (k v)
	       (if (or (member-if (lambda (thing)
				    (and (stringp thing)
					 (string-match (concat "\\<" value "\\>") thing))) v)
		       (string-match value (symbol-name k)))
		   (push (symbol-name k) result)))
	     gimp-dump)
    (sort result 'string<)))

(gimp-defcommand gimp-apropos (&optional query)
  "Search pdb for submatch of QUERY."
  (interactive)
  (let* ((query (or query (read-from-minibuffer "Apropos query: " )))
	 (new-contents
          (mapconcat
           (lambda (proc)
	     (replace-regexp-in-string
	      (if (not (string= query ""))
		  query
		"^ ")		;do not bother when matching anything
	      (lambda (m)
		(propertize m
			    'font-lock-face
			    'gimp-less-shy-face))
	      (propertize proc 'mouse-face 'highlight
			  'font-lock-face
			  (if (member (read proc)
				      gimp-help-visited-pages)
			      'gimp-visited-procedure-face
                           'default))))
           (gimp-real-apropos query) "\n")))
    (if (> (length new-contents) 0)
	(gimp-help-wrapper
         `(gimp-apropos ,query)
	 (insert
          (propertize (format "GIMP procedures apropos %S\n"
                              (if (string= "" query)
                                  'anything
                                query))
		      'font-lock-face 'gimp-level1-face)
          (propertize (make-string (window-width) ?=)
                      'font-lock-face 'gimp-shy-face)
          "\n"
          new-contents)
         (goto-char (point-min))
	 (forward-line 2))
      (message "No match"))))

(defun gimp-help-back ()
  "Select last shown GIMP Help page.

Go back one page in GIMP Help, and set current page to be the last one
hereafter."
  (interactive)
  (setf (aref (get 'gimp-help-ring 'gimp-help-positions)
              (- (length gimp-help-ring)
                 (get 'gimp-help-ring 'index)))
        (point))
  (eval
   (nth
    (put 'gimp-help-ring
         'index
         (min
          (1- (length gimp-help-ring))
          (1+ (get 'gimp-help-ring 'index))))
    gimp-help-ring))
  (goto-char
   (aref (get 'gimp-help-ring 'gimp-help-positions)
         (- (length gimp-help-ring)
            (get 'gimp-help-ring 'index)))))

(defun gimp-help-forward ()
  "Select last shown GIMP Help page.

Go back one page in GIMP Help, and set current page to be the last one
hereafter."
  (interactive)
  (setf (aref (get 'gimp-help-ring 'gimp-help-positions)
              (- (length gimp-help-ring)
                 (get 'gimp-help-ring 'index)))
        (point))
  (eval
   (nth
    (put 'gimp-help-ring
         'index
         (max
          0
          (1- (get 'gimp-help-ring 'index))))
    gimp-help-ring))
  (goto-char
   (aref (get 'gimp-help-ring 'gimp-help-positions)
         (- (length gimp-help-ring)
            (get 'gimp-help-ring 'index)))))

(defun gimp-describe-procedure-at-point ()
  (interactive)
  (let ((proc (gimp-procedure-at-point)))
    (when proc
      (gimp-echo-procedure-description proc))))

(gimp-defcommand gimp-describe-procedure (&optional proc)
  "Describe function in current sexp, or the one at point.
This is a full description, similar to the one in the gimp pdb browser.
The description is shown in the *GIMP Help* buffer.

If the procedure is menu-registered, the submenus shown can be followed (by
clicking or pressing ENTER), to have a shortcut into the command `gimp-menu'
for like procedures.

You can also follow email addresses and urls.

One can TAB through these followable fields.

Use `outline-mode' commands to navigate and fold stuff.

Optional argument PROC is a string identifying a procedure."
  (interactive)
  (let* ((sym
	  (read
	   (or
            proc
            (gimp-procedure-at-point t)
            (car (member (symbol-name
                          (gimp-fnsym-in-current-sexp)) gimp-pdb-cache))
            (completing-read "Procedure: " gimp-pdb-cache nil t
                             (if (gethash (symbol-at-point) gimp-dump)
                                 (symbol-name (symbol-at-point)))))))
         (count 0))
    (add-to-list 'gimp-help-visited-pages sym)
    (gimp-help-wrapper `(gimp-describe-procedure ,(symbol-name sym))
		       (insert
			(or (let ((desc
				   (format
				    "%s%s %s\n%s\n%s\n%s\n\n%s"
				    (propertize "* " 'font-lock-face 'gimp-shy-face)
                                    (propertize "GIMP proc: "
                                                'font-lock-face 'gimp-level1-face)
				    (propertize (symbol-name sym)
						'field 'procedure
						'mouse-face 'highlight
						'help-echo "i : insert symbol at REPL\ns : search source code for symbol"
						'font-lock-face 'gimp-level1-face)
				    (propertize (make-string (window-width) ?=)
                                                'font-lock-face 'gimp-shy-face)
				    (let ((menu (cadr (assoc (symbol-name sym) gimp-menu))))
				      (if (null menu)
					  ""
					(setq menu (nreverse (split-string menu "/")))
					(concat
					 (mapconcat
					  'identity
					  (reverse (mapcar
						    (lambda (submenu) ;
						      (propertize
						       submenu
						       'mouse-face 'highlight
						       'field 'submenu
						       'help-echo
						       "Find more plugins under this submenu"
						       'font-lock-face 'gimp-menu-face))
						    (cdr menu)))
					  (propertize "/" 'font-lock-face 'gimp-shy-face))
                                         (propertize "/" 'font-lock-face 'gimp-shy-face)
                                         (propertize (car menu)
                                                     'font-lock-face 'gimp-terminal-menu-face))))
                                    
				    (let ((case-fold-search t))
				      (propertize (replace-regexp-in-string
				       "'\\([[:alpha:]-]+\\)'"
				       (lambda (match)
					 (if (member (match-string 1 match) gimp-pdb-cache)
					     (propertize match
							 'field 'function
							 'help-echo "Follow link"
							 'mouse-face 'highlight
							 'font-lock-face
                                                         (if
                                                             (member (read (match-string 1 match))
                                                                     gimp-help-visited-pages)
                                                             'font-lock-builtin-face
                                                           'font-lock-function-name-face))
					   match
					   ))
				       (replace-regexp-in-string
					gimp-url-regexp
					(lambda (match)
					  (propertize match
						      'field                'url
						      'help-echo            "Follow link"
						      'mouse-face           'highlight
						      'font-lock-face       'gimp-link-face))
					(replace-regexp-in-string
					 gimp-email-regexp
					 (lambda (match)
					   (propertize match
						       'field                'email
						       'help-echo            "Mail author"
						       'mouse-face           'highlight
						       'font-lock-face       'gimp-link-face))
					 (gimp-procedure-description sym))))))

				    (mapconcat
				     (lambda
				       (argument)
				       (let ((desc2 (car (cddr argument))))
					 (when (string-match "{.*}" desc2)
					   (setq desc2
						 (replace-regexp-in-string " { " ":\n\n" desc2))
					   (setq desc2
						 (replace-regexp-in-string "}" "" desc2))
					   (setq desc2
						 (mapconcat
						  (lambda (item)
						    (if
							(string-match
							 "\\(.+\\) \\(([0-9]+)\\)" item)
							(format "      %-21s%2s"
								(match-string 1 item)
								(match-string 2 item))
						      item))
						  (split-string desc2 "\\(, \\|\n\\)") "\n")))
					 (format "%s %s\n%s\n%s"
                                                 (propertize "**" 'font-lock-face 'gimp-shy-face)
                                                 (propertize
                                                  (format "%-2d %-20s %50s"
                                                          (incf count)
                                                          (replace-regexp-in-string "GIMP_PDB_" "" (cadr argument))
                                                          (car argument))
                                                  'font-lock-face
                                                  'gimp-level2-face)
                                                 desc2
						 (let ((scraped (gimp-arg-from-scraped-registration (1- count) 
												    (symbol-name sym))))
						   (if scraped
						       (concat "\nDefaults:\n"
						       (replace-regexp-in-string 
							"(\\|)"
							(lambda (m)
							  (propertize m
								      'font-lock-face 
								      'gimp-less-shy-face))
							(propertize (format "%S" scraped) 
								    'font-lock-face 
								    'font-lock-variable-name-face)))
						     "")))))
				     (gimp-get-proc-args sym)
				     "\n\n"))))
			      desc)))
		       (setq gimp-current-procedure sym)))
  (goto-char (point-min)))

 ;; Internal information retrieval
(defun gimp-get-proc-arg (proc arg)
  (nth arg (gimp-get-proc-args proc)))

(defun gimp-get-proc-description (proc)
  (gethash proc gimp-dump))

(defun gimp-get-proc-args (proc)
  (let ((all-args (nth 6 (gimp-get-proc-description proc))))
    (if (and (eq (gimp-get-proc-type proc)   
                 'fu)
             (string= (caar all-args) 
                      "run-mode"))
        (cdr all-args)
      all-args)))

(defun gimp-get-proc-arg-descriptive-name (sym pos)
  (caddr (gimp-get-proc-arg sym pos)))

(defun gimp-get-proc-type (sym)
  (let ((type (nth 5 (gimp-get-proc-description sym))))
    (cond 
     ((string= type "Temporary Procedure") 'fu)
     ((string= type "Internal GIMP procedure") 'internal)
     ((string= type "GIMP Plug-In") 'plug-in)
     ((string= type "GIMP Extension") 'extension)
     (t 'other))))

(defun gimp-get-closure-code (sym)
  (read
   (gimp-eval-to-string
    (apply 'format
	   "(let ((code (get-closure-code %s)))\
 (if code (cons '%s (cadr code)) 'nil))"
	   (make-list 3 sym)))))

(defun gimp-procedure-description (sym)
  (let ((info (gimp-get-proc-description sym)))
    (concat (car info)
            "\n\n"
            (with-temp-buffer 
              (insert (cadr info))
              (fill-paragraph t)
              (buffer-substring (point-min)
                                (point-max)))
            "\n\nAuthor(s): "
            (replace-regexp-in-string   
             "@@"
             "@"
             (nth 2 info))
            "\nCopyright: "
            (replace-regexp-in-string 
             "@@"
             "@"
             (nth 3 info))
            ", "
            (nth 4 info))))

 ;; Completion
(defcustom gimp-complete-fuzzy-p t
  "Perform fuzzy completion?

This is a special implementation of fuzziness to aid in
completion of hyphen-separated symbols that are frequent in lisp.
It lets you type in the first letter(s) of the combining parts of
such symbol.  Of course, the basic case of consecutive characters
found anywhere in a target string will match.

Examples of the implementation of fuzziness:

s-f-c-s => script-fu-coffee-stain
stain => script-fu-coffee-stain
scr-f- => script-fu-
s-f--logo => suggestions for script-fu-neon-logo,
script-fu-comic-logo, script-fu-chalk-logo ...

Note on comparison with other fuzzy algorithms:
 
It does *not* match like symbols, as do other implementations.
Neither does e.g. \"sif\" match \"script-fu\".

You can toggle this variable at any time with the command
`gimp-toggle-fuzzy-completion'.

Its state is indicated in the mode-line."
  :group 'gimp
  :type 'boolean)

(defcustom gimp-completion-rules

  '(((lambda (desc name type)
       (string-match "FLOAT" type))
     . nil)

    ((lambda (desc name &rest type)
       (string-match "filename" name))
     . (lambda (&rest ignore)
         (interactive)
         (when (not (gimp-in-string-p))
           (insert (format "%S" (expand-file-name "~/")))
           (backward-char 1))
         (comint-dynamic-complete-filename)
         'discard))

    ((lambda (desc name type)
       (and (not (gimp-in-string-p))
            (string-match "STRING" type)))
     . (lambda (&rest ignore)
         (when gimp-insert-quotes-for-strings-p 
           (insert "\"\"")
           (forward-char -1))
         (gimp-make-completion (list desc name type))))

    ((lambda (desc name type)
       (string-match "{.*}" desc))
     . (lambda (desc &rest ignore)
         (split-string desc "\\(.*{ *\\|, \\|([0-9]+)\\|}.*\\|@\\)" t)))

    ((lambda (desc name type)
       (and (string-match "gradient" desc)
            (string= type "GIMP_PDB_STRING")))
     . (lambda (&rest ignore)
         (in-gimp (cadr (gimp-gradients-get-list "")))))

    ((lambda (desc name type)
       (string-match "paint method" desc))
     . (lambda (&rest ignore)
         (in-gimp (cadr (gimp-context-list-paint-methods)))))

    ((lambda (desc name type)
       (or (string-match "font" name)
	   (string= "The name of the font" desc)))
     . (lambda (&rest ignore)
         gimp-fonts-cache))

    ((lambda (desc name type)
       (string-match  "pattern" name))
     . (lambda (&rest ignore)
         gimp-patterns-cache))

    ((lambda (desc name type)
       (string= "procedure-name" name))
     . (lambda (&rest ignore)
         gimp-pdb-cache))

    ((lambda (desc name type)
       (string= "run-mode" name))
     .
     ("RUN-INTERACTIVE" "RUN-NONINTERACTIVE"))

    ((lambda (desc name type)
       (or (string= name "toggle")
           (string-match "\\((TRUE or FALSE)\\|toggle\\)"
                         desc)))
     . ("TRUE" "FALSE"))

    ((lambda (desc name type)
       (string-match "The brush name" desc))
     . (lambda (&rest ignore)
         gimp-brushes-cache))

    ((lambda (desc name type)
       (string-match "palette" desc))
     . (lambda (&rest ignore)
         gimp-palettes-cache))

    ((lambda (desc name type)
       (message "%S %S %S" desc name type)
       (string= type "GIMP_PDB_IMAGE"))
     . (lambda (&rest ignore)
	  (in-gimp
	   (mapcar number->string
                  (vector->list (cadr (gimp-image-list)))))))
    
    ((lambda (desc name type)
       (string-match "color" name))
     . (lambda (&rest ignore)
	 (when (and (not (gimp-buffer-window
			  "*Colors*"))
		    (y-or-n-p "List colors? "))
	   (list-colors-display)
	   nil)))
    
    ((lambda (desc name type)
       (string= "operation" name))
     . ("CHANNEL-OP-ADD"
	"CHANNEL-OP-SUBTRACT"
	"CHANNEL-OP-REPLACE"
	"CHANNEL-OP-INTERSECT"))

    ((lambda (desc name type)
       (string-match "STRING" type))
     . (lambda (&rest ignore)))

    ((lambda (desc name type)
       (string= name "icon-type"))
     . ("ICON-TYPE-STOCK-ID"
        "ICON-TYPE-INLINE-PIXBUF"
        "ICON-TYPE-IMAGE-FILE"))

    ((lambda (desc name type)
       (string= name "decompose-type"))
     . ("RGB" "Red" "Green" "Blue" "RGBA" "HSV" "Hue"
        "Saturation" "Value" "HSL" "Hue (HSL)" "Saturation (HSL)"
        "Lightness" "CMY" "Cyan" "Magenta" "Yellow" "CMYK"
        "Cyan_K" "Magenta_K" "Yellow_K" "Alpha" "LAB"
        "YCbCr_ITU_R470" "YCbCr_ITU_R709" "YCbCr ITU R470 256"
        "YCbCr ITU R709 256"))

    ((lambda (desc name type)
       (string= "GIMP_PDB_DRAWABLE" type))
     .
     nil)
    ((lambda (desc name type)
       t)
     . (lambda (&rest ignore)
         gimp-oblist-cache)))
  
  "Ruleset for deciding the completion to perform by `gimp-make-completion'.

A rule is list of the form (MATCH-FUNCTION . ACTION-OR-LIST)

These rules are checked in order until a match is found.

If ACTION-OR-LIST is a function, it must provide a list of completion
candidates.

If ACTION-OR-LIST is a list of the form (\"ELT1\" \"ELT2\" ...) this list will
serve as the candidate pool.

Both the MATCH-FUNCTION and the ACTION-OR-LIST (if an action) take three
arguments pertaining to the argument, in order:

\(description name type)"

  :group 'gimp
  :type '(alist :key-type function :value-type
		(choice (function :tag "Action")
			(sexp :tag "List"))))

(defun gimp-indent-and-complete ()
  "Indent and complete function or argument at point."
  (interactive)
  (progn
    (lisp-indent-line)
    (gimp-complete)
    (gimp-echo)))

(defun gimp-current-arg ()
  "Return positional information on the current argument.

The output is a list of (BEG END PATTERN), where BEG and END are
its buffer positions and PATTERN is the argument itself."
  (with-syntax-table scheme-mode-syntax-table
    (let* ((end
            (save-excursion
              (cond ((eobp)
                     (point))
                    ((gimp-in-string-p)
                     (while 
                         (memq (char-syntax (following-char)) '(?w ?_ ?\ ))
                       (forward-char 1))
                     (point))
                    ((memq (char-syntax (char-after (point))) '(?) ?\" ?( ? ))
                     (point))
                    (t (while 
                           (memq (char-syntax (following-char)) '(?w ?_))
                         (forward-char 1))
                       (point)))))
           (beg
            (save-excursion
              (cond ((gimp-in-string-p)
                     (re-search-backward "[^\\]\"" nil t)
                     (forward-char 2)
                     (point))
                    ((memq (char-syntax (char-before (point))) '(?\( ?\ ? ))
                     (point))
                    (t (while 
                           (memq (char-syntax (char-before)) '(?w ?_))
                         (forward-char -1))
                       (point)))))
           (pattern (buffer-substring-no-properties beg end)))
      (list beg end pattern))))

(defun gimp-complete-savvy (&optional lst)
  "Perform completion on script-fu symbol preceding point.
Compare that symbol against the known script-fu symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.
Optional argument LST specifies a list of completion candidates."
  (let ((window (get-buffer-window "*Completions*" 0)))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window)))
;; If this command was repeated, and
;; there's a fresh completion window with a live buffer,
;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))
;; Do completion.
      (multiple-value-bind (beg end pattern) (gimp-current-arg)
          (let* ((lst (or lst gimp-oblist-cache))
	     (completion
	      (if (not gimp-complete-fuzzy-p)
		  (try-completion pattern lst nil)
		(or (gimp-try-fuzzy-completion pattern lst) pattern))))
	(cond ((eq completion t))
	      ((null completion)
	       (ding))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion)
;; Don't leave around a completions buffer that's out of date.
	       (let ((win (get-buffer-window "*Completions*" 0)))
		 (if win (with-selected-window win (bury-buffer)))))
	      (t
	       (let ((minibuf-is-in-use
		      (eq (minibuffer-window) (selected-window))))
		 (unless minibuf-is-in-use
		   (message "Making completion list..."))
		 (let ((lst2
			(if gimp-complete-fuzzy-p
			    (gimp-all-fuzzy-completions
			     pattern
			     lst)
			  (all-completions pattern lst nil))))
		   (if (not gimp-complete-fuzzy-p)
		       (setq lst2 (sort lst2 'string<)))
		   (if (> (length lst2) 1)
		       (progn
;; (with-output-to-temp-buffer "*Completions*"
;;   (display-completion-list nil))
					;set up a good buffer (with
					;all them hooks (orig code:)
			 (with-output-to-temp-buffer "*Completions*"
			   (display-completion-list lst2))
			 (gimp-highlight-matches completion
                                                 "*Completions*"))

;; Don't leave around a completions buffer that's
;; out of date.
		     (if (not gimp-complete-fuzzy-p)
			 (let ((win (get-buffer-window "*Completions*" 0)))
			   (if win (with-selected-window win (bury-buffer))))
		       (when (= 1 (length lst))
			 (delete-region beg end)
			 (insert (car lst2))))))
		 (unless minibuf-is-in-use
		   (message "Making completion list...%s" "done")
		   )))))))))

(defun gimp-complete-oblist (&optional discard)
  "Function that always just uses the oblist to complete the symbol at point.

Pushed into `hippie-expand-try-functions-list'."
  (interactive)
  (case major-mode
    ((gimp-mode inferior-gimp-mode)
     (gimp-complete-savvy  
      gimp-oblist-cache))))

(push 'gimp-complete-oblist hippie-expand-try-functions-list)

(defun gimp-highlight-matches (pattern buffer)
  "Propertize PATTERN in the BUFFER."
  (unless (string= pattern "")
    (save-window-excursion
      (switch-to-buffer-other-window buffer)
      (let (buffer-read-only)
        (while (re-search-forward (gimp-make-fuzzy-match-re pattern) nil t)
          (replace-match
           (propertize (match-string 0)
                       'font-lock-face
                       'gimp-level2-face)))
        (goto-char (point-min))
        (forward-line 2)
        (insert
         "Press C-cr to toggle fuzzy completion\n")))))

(defun gimp-completable-at-p ()
  "Check whether thing at p is completable."
  (let ((fun (gimp-fnsym-in-current-sexp)))
    (gethash fun gimp-dump)))

(defun gimp-complete ()
  "Main completion function."
  (let ((fun (gimp-fnsym-in-current-sexp))
        (pos (gimp-position))
        (gimp-command
         (save-excursion
           (cadr (gimp-string-match "^,\\([[:alpha:]-]*\\)"
                                    (comint-get-old-input-default))))))
    (cond
     (gimp-command
      (gimp-complete-savvy gimp-shortcuts))
     ((> pos (length (gimp-get-proc-args fun)))
      (point))
     ((gimp-completable-at-p)
      (let* ((desc (gimp-get-proc-arg fun (1- pos)))
             (list (gimp-make-completion desc))
             (scraped-arg (gimp-arg-from-scraped-registration
                           (1- pos)
                           (symbol-name fun)))
             (question (format "%s %s: "
                               (caddr desc)
                               (or scraped-arg ""))))
        (if (string= (car desc)
                     "option")
            (setq list
                  (or scraped-arg list)))
        (cond
         ((eq list 'discard)
          nil)
         ((functionp list)
          (funcall list))
         (list
          (gimp-complete-savvy list))
         (t (let ((answer (read-from-minibuffer
                           question
                           (if scraped-arg
                               (case
                                   (read (cadr desc))
                                 (GIMP_PDB_FLOAT (number-to-string (cadar scraped-arg)))
                                 (GIMP_PDB_COLOR (if (numberp (car scraped-arg))
                                                     (format "'%S" scraped-arg)
                                                   (car scraped-arg)))
                                 (t (format "%S" (car scraped-arg))))
                             nil))))
              (insert (replace-regexp-in-string "\\(^\"\\|\"$\\)" "" answer)))))))
     (t
      (gimp-complete-savvy gimp-oblist-cache)))))

(defun gimp-local-completion ()
  (let* ((desc (gimp-get-proc-arg 
                (gimp-fnsym-in-current-sexp)
                (1- (gimp-position))))
         (list (gimp-make-completion desc)))))

(defun gimp-make-completion (desc)
  (destructuring-bind (name type desc) desc
    (let ((action
           (cdr (assoc-if
                 (lambda (rule)
                   (if (functionp rule)
                       (apply rule (list desc name type))
		     nil))
                 gimp-completion-rules))))
      (if (functionp action)
          (apply action (list desc name type))
        action))))

(gimp-defcommand gimp-toggle-fuzzy-completion ()
  "Toggle fuzzy completion.

See variable `gimp-complete-fuzzy-p' for a bit more detailed information."
 (interactive)
 (setq gimp-complete-fuzzy-p 
	(not gimp-complete-fuzzy-p))
  (if (eq last-command 'gimp-indent-and-complete)
      (call-interactively 'gimp-indent-and-complete)))

(defun gimp-make-fuzzy-match-re (pattern)
  (let ((re (replace-regexp-in-string "-" "[^-]*-" pattern)))
    (if (string= pattern re)
	pattern
      re)))
    
(defun gimp-all-fuzzy-completions (pattern list)
  (mapcar 'car 
	  (gimp-all-fuzzy-completion-data pattern list)))

(defun gimp-all-fuzzy-completion-data (pattern list)
  (let* ((re (gimp-make-fuzzy-match-re pattern)))
    (sort* 
     (loop for s in list
	   for match = (or (string-match re s)
			   (string-match pattern s))
	   when match
	   collect (list s match 
			 (length (match-string 0 s))
			 (length  s)
			 (match-string 0 s)))
	   #'<
	   :key 'cadddr)))

(defun gimp-try-fuzzy-completion (pattern list)
  (let* ((max-lisp-eval-depth 30000)
	 (completions (gimp-all-fuzzy-completion-data pattern list)))
    (if (null completions) 
	(error "No completions")
      (if (fboundp 'icicle-expanded-common-match)
	  (icicle-expanded-common-match 
	   (gimp-make-fuzzy-match-re pattern)
	   (mapcar 'car completions))
	(if (gimp-test-recursively
	     (lambda (one two)
	       (string= (nth 4 one)
			(nth 4 two)))
	     completions
	     100)
	    (if (= (length completions) 1)
		(caar completions)
	      (if (nth 4 (car completions))
		  (try-completion (nth 4 (car completions))
				  (mapcar 'car completions))))
	  nil)))))
 ;; Shortcuts
(gimp-defcommand gimp-shortcuts (&optional terse)
  "Show interactive commands in the REPL.
Optional argument TERSE means only show that I am there to help you."
  (interactive)
  (if (eq major-mode 'inferior-gimp-mode)
      (progn
        (if terse (insert "Type ," (gimp-make-input-field "shortcuts") " for list of commands")
          (insert "Available commands:\n  "
                  "\n  ,"
                  (mapconcat 'gimp-make-input-field gimp-shortcuts "\n  ,")
                  "\n  "))
        (insert (propertize "\n> "
                            'font-lock-face 'comint-highlight-prompt
                            'field 'output
                            'inhibit-line-move-field-capture t
                            'rear-nonsticky t))
        (let ((pmark (process-mark (gimp-proc))))
          (setq comint-save-input-ring-index comint-input-ring-index)
          (setq comint-input-ring-index nil)
	  ;; Update the markers to discard this 'input'.
          (set-marker comint-last-input-start pmark)
          (set-marker comint-last-input-end (point))
          (set-marker (process-mark (gimp-proc))
                      (point))))
    (error "Not in *GIMP* buffer")))

(defun gimp-make-input-field (arg)
  (propertize arg 'help-echo 
	      (gimp-string-match "^\\(.*$\\)" 
				 (documentation
				  (intern-soft
				   (format "gimp-%s" arg))) 1)
              'mouse-face 'highlight
              'field 'input
              'font-lock-face 'gimp-link-face))
 ;; Doc echoing
(defun gimp-docstring (sym)
  (if (and 
       (eq (gimp-get-proc-type sym) 
           'fu)
       (eq this-command last-command))
      (progn 
        (setq this-command 'other-command)
        (gimp-get-closure-code sym))
    (cons sym 
	  (mapcar (lambda (arg)
		    (list (read (car arg))
			  (read (replace-regexp-in-string 
				 "GIMP_PDB_"
				 "" (cadr arg))))) 
		  (gimp-get-proc-args sym)))))

(defun gimp-echo (&optional sym)
  "Echo function  and argument information for SYM.
The echo takes the form of (function (name-1 TYPE)...(name-n TYPE)), where the
argument at point is highlighted."
  (interactive )
  (unless (gimp-in-string-p)
    (let ((result))
    (catch 'a
      (let* ((sym (or sym (gimp-fnsym-in-current-sexp)))
	     (str (symbol-name sym))
	     cache-resp
	     (pos (gimp-position)))
	
	(cond ((gethash sym gimp-dump)
	       ;; Get it
	       (setq cache-resp
		     (gimp-docstring (read str))))
	      ((unless (string-match "define\\(?:-macro\\)?\\|let" str)
                                        ;`scheme-get-current-symbol-info'
                                        ;"fails" in these cases.
		 (let
		     ((info
		       (scheme-get-current-symbol-info)))
		   (if (and info (listp (read info))) ;the actual condition
                                        ;for cond clause
		       (progn
			 (setq cache-resp (read info))
			 (setq result (gimp-string-match "(.*)\\(.*\\)" info 1))
			 t)		;break
		     nil))))
	      (t
	       ;; Get it (unless we have it already)
	       (unless cache-resp
		 (setq cache-resp
		       (gimp-get-closure-code sym))
		 (when (not (consp cache-resp))
		   (setq cache-resp nil)))))

	(when (and cache-resp 
                   (not (eq 'gimp-error (car cache-resp)))) ;this may be 
                                                     ;an error
                                                     ;in gimp-cl
	  (setq cache-resp
 		(mapcar (lambda (item) (format "%s" item))
			(dotted-to-list cache-resp)))
	  (setf (car cache-resp)
		(propertize str 'face 'font-lock-keyword-face))
	  ;; Show it
	  (let ((this-arg (nth pos cache-resp)))
	    (when this-arg
	      (when (> pos 0)
		(let ((arg (nth pos cache-resp)))
		  (setf (nth pos cache-resp)
			(if (string-match "^\. " arg)
			    (concat ". " (propertize (substring arg 2) 'face 'highlight))
			  (propertize arg 'face 'highlight)))))
	      (message "(%s)%s" (mapconcat 'identity cache-resp " ")
		       (or result ""))
	      (when (> pos 0)
		(set-text-properties 0 (length this-arg)
				     nil (nth pos cache-resp)))))))))))

(defun gimp-echo-procedure-description (sym)
  "Echo short description for SYM."
  (message "%s" (gimp-procedure-description sym)))

(defun gimp-describe-this-arg ()
  "Echo description for argument or procedure at point."
  (interactive)
  (let* ((sym (gimp-without-string (gimp-fnsym-in-current-sexp))))
    (if (member (symbol-name sym) gimp-pdb-cache)
	(if (eql sym (symbol-at-point))
	    (gimp-echo-procedure-description sym)
	  (message "%s %s"
                   (gimp-get-proc-arg-descriptive-name sym (1- (gimp-position)))
                   (if (eq (gimp-get-proc-type sym) 'fu)
                       (gimp-arg-from-scraped-registration (1- (gimp-position))
                                                           (symbol-name sym))
                     "")))
      (message "%S: No information available" sym))))

 ;; Source look-up
(defun gimp-code-search-fu (proc)
  "Search for definition of script-fu procedure PROC."
  (grep (format "grep -rnH \"( *define\\( (\\| +\\)%s\\([^a-z0-9!?<>-]\\|$\\)\" %s %s"
		proc
		(shell-quote-argument (format "%s/scripts/" (gimp-data-dir)))
                (shell-quote-argument (format "%s/scripts/" (gimp-dir))))))

(defun gimp-code-search-plug-in-file (proc)
  "Search for a plug-in file containing plug-in PROC.
Needs the variable `gimp-src-dir' to be set."
  (grep
   (format "grep -nHri \"\\\"%s\\\"\" %s*"
           proc
           (shell-quote-wildcard-pattern (concat (gimp-src-dir) "/plug-ins/")))))

(defun gimp-code-search-core-function (proc)
  "Search for the definition of core procedure PROC.
Needs the variable `gimp-src-dir' to be set."
  (let ((proc
         (replace-regexp-in-string
          "-" "_" proc)))
    (grep (format "grep -rnH \"^%s[^a-z0-9_]\" %s*"
                  proc
                  (shell-quote-wildcard-pattern (concat (gimp-src-dir) "/libgimp*/"))))))

(gimp-defcommand gimp-code-search (&optional proc)
  "Scavenge source for a procedure.

This admittedly is a poor implementation, and you should delve
into etags and find-tag."
  (interactive)
  (let ((proc 
         (or proc 
             (completing-read 
              "Search code for procedure: "
              gimp-pdb-cache nil t
              (let ((p (or (gimp-procedure-at-point)
                           (and
                            (gethash (gimp-fnsym-in-current-sexp) gimp-dump)
                            (gimp-fnsym-in-current-sexp)))))
                (if p (symbol-name p) nil))))))
    (case (gimp-get-proc-type (read proc))
      (internal (gimp-code-search-core-function proc))
      (extension (gimp-code-search-plug-in-file proc))
      (plug-in (gimp-code-search-plug-in-file proc))
      (fu (gimp-code-search-fu proc))
      (t (error "Look up-for %s unimplemented" proc)))))

 ;; Snippets
(snippet-with-abbrev-table
 'gimp-mode-abbrev-table
 ("reg" . (format 
           "(define (script-fu-$${name})\n$>$.)
 

\(script-fu-register \"script-fu-$${name}\"
                    _\"$${blurp (use _ before shortcut letter!) }\"
                    _\"$${help}\"
                    \"$${%s} ($${%s})\"
                    \"$${%s}\"
                    \"$${%s}\"
                    \"\")

\(script-fu-menu-register \"script-fu-$${name}\"
                         _\"<Toolbox>/Xtns/Script-Fu\")" 
           user-full-name
           user-mail-address
           user-full-name
           (format-time-string "%Y-%m-%d")))
 ("sft" . "SF-TOGGLE\t_\"$${On or Off?}\"\t$${TRUE}$>")
 ("sfv" . "SF-VALUE\t_\"$${text}\"\t\"$${STRING}\"$>")
 ("sfs" . "SF-STRING\t_\"$${text}\"\t\"$${STRING}\"$>")
 ("sfr" . "SF-RADIO\t_\"$${Choose: (radio) }\"\t'($${option1...})$>")
 ("sfa" .
  "SF-ADJUSTMENT\t_\"$${Adjust: }\"\t'($${initval} $${lower} $${upper}\
 $${step_inc} $${page_inc} $${digits} $${type (0: slider, 1: spinner) })$>")
 ("sfc" . "SF-COLOR\t_\"$${Color: }\"\t\"$${#000}\"$>")
 ("sff" . "SF-FILENAME\t_\"$${Filename: }\"\t\"$${~/Pictures}\"$>")
 ("sfd" . "SF-DIRNAME\t_\"$${Dirname: }\"\t\"$${~/Pictures/}\"$>")
 ("sfo" . 
  "SF-OPTION\t_\"$${Choose: (combo)}\"\t'(\"$${opt1}\" \"$${opt2...}\")$>")
 ("sfe" . 
  "SF-ENUM\t_\"$${Choose}\"\t'(\"$${enum name}\" \"$${initval}\")$>")
 ("sfb" .
  "SF-BRUSH\t_\"$${Brush:}\"\t'(\"$${brush}\" $${opacity} $${spacing}\
 $${NORMAL})$>" )
 ("sfp" . "SF-PATTERN\t_\"$${Pattern:}\"\t\"$${Pine}\"$>")
 ("sfg" . "SF-GRADIENT\t_\"$${Gradient:}\"\t\"$${FG to BG (RGB)}\"$>")
 ("sfpl" . "SF-PALETTE\t_\"$${Palette:}\"\t\"$${Web}\"$>")
 ("sffont" . "SF-FONT\t_\"$${Font:}\"\t\"$${Helvetica}\"$>")
 ("sfi" . "SF-IMAGE\t\"Image\"\t1$>")
 ("sfdr" . "SF-DRAWABLE\t\"Drawable\"\t1$>")
 ("sfl" . "SF-LAYER\t\"Layer\"\t1$>")
 ("sfvec" . "SF-VECTORS\t\"Vector\"\t'($${syntax undocced})$>"))

(defconst gimp-snippets 
  '(("reg" . "Definition and registration set-up")
    ("sft" . "SF-TOGGLE...")
    ("sfv" . "SF-VALUE...")
    ("sfs" . "SF-STRING...")
    ("sfr" . "SF-RADIO...")
    ("sfa" . "SF-ADJUSTMENT...")
    ("sfc" . "SF-COLOR...")
    ("sff" . "SF-FILENAME...")
    ("sfd" . "SF-DIRNAME...")
    ("sfo" . "SF-OPTION...")
    ("sfe" . "SF-ENUM...")
    ("sfb" . "SF-BRUSH...")
    ("sfp" . "SF-PATTERN...")
    ("sfg" . "SF-GRADIENT...")
    ("sfpl" . "SF-PALETTE...")
    ("sffont" . "SF-FONT...")
    ("sfi" . "SF-IMAGE...")
    ("sfdr" . "SF-DRAWABLE...")
    ("sfl" . "SF-LAYER...")
    ("sfvec" . "SF-VECTORS..."))
  "Reference list snippets ")
 
(gimp-defcommand gimp-list-snippets ()
  "List predefined snippets for GIMPMode."
  (interactive)
  (gimp-help-wrapper
   '(gimp-list-snippets)
   (insert
    (propertize "List of predefined GIMP Mode snippets"
                'font-lock-face 'gimp-level1-face)
    "\nType the abbreviations on the left in a GIMP Mode buffer, and hit ENTER\n"
    (propertize (make-string (window-width) ?=)
                'font-lock-face 'gimp-shy-face)
    "\n"
    (mapconcat
     (lambda (i)
       (format "%-17s%s"
 	       (car i)
 	       (cdr i)))
     gimp-snippets "\n"))
   (goto-char (point-min))))

 ;; Misc interactive commands
(gimp-defcommand gimp-selector (char)
  "GIMP buffer switcher similar to `slime-selector.
Argument CHAR is used to choose between buffers.'."
  (interactive "cSwitch to gimp buffer [rldhc?]: ")
  (case char
    (?l (let ((buffer (car
		       (member-if
			(lambda (b)
			  (with-current-buffer b
			    (eq major-mode 'gimp-mode)))
			(buffer-list)))))
          (if buffer
              (switch-to-buffer buffer)
            (error "No living gimp-mode buffers to switch to"))))
    (?h (gimp-help))
    (?r (call-interactively 'run-gimp))
    (?c (if (eq (process-status gimp-cl-proc) 'open)
            (switch-to-buffer gimp-cl-buffer-name)
          (gimp-cl-connect)))
    (?d (call-interactively 'gimp-documentation))
    (?? (message "i = inferior gimp buffer; l: last lisp buffer; d: online documentation; h: help.")
        (sit-for 3)
        (call-interactively 'gimp-selector))
    (t (call-interactively 'gimp-selector))))

(gimp-defcommand gimp-load-script (&optional script)
  "Load a SCRIPT into the scheme image."
  (interactive)
  (let* ((script (or script (read-file-name "Load script: " (format "%s/scripts/" (gimp-dir)))))
         (command (format "(load %S)"  (expand-file-name script))))
    (if (eq this-command 'gimp-send-input)
        command
      (message "%s" command))))

(gimp-defcommand gimp-report-bug (subject)
  "Send a bug report on GIMP Mode."
  (interactive "sGIMP Mode Bug Subject: ")
  (let ((from-buffer (current-buffer)))
    (mail nil (rot13 "avryf.tvrfra@tznvy.pbz") 
          subject)

    (insert (format 
             "
This is a bug report for GIMP mode (NOT for the GIMP itself!) .
This bug report will be sent to the GIMP mode maintainer,
not to your local site managers!

Please describe exactly what actions triggered the bug
and the precise symptoms of the bug (it may also be
helpful to include an *EXAMPLE FILE*!):

GIMP-mode version: %s

In %s\n\n\n"
             (gimp-gimp-mode-version)
             (emacs-version)))
    (save-excursion
      (insert (format "\n\nMajor mode: %s\n"
                      (format-mode-line
                       (buffer-local-value 'mode-name from-buffer)
                       nil nil from-buffer)))
      (insert "\n")
      (insert "Minor modes in effect:\n")
      (dolist (mode minor-mode-list)
        (and (boundp mode) (buffer-local-value mode from-buffer)
             (insert (format "  %s: %s\n" mode
                             (buffer-local-value mode from-buffer)))))
      (insert "\n"))))

(gimp-defcommand gimp-refresh-scripts ()
  "Refresh script-fu scripts."
  (interactive)
  (let* ((command "(car (script-fu-refresh RUN-INTERACTIVE))"))
    (if (eq this-command 'gimp-send-input)
        command
      (message "Refreshing scripts...")
      (if (gimp-eval command)
          (message "Completed!")
        (message "Failed!")))))

(gimp-defcommand gimp-open-image ()
  "Open image(s) by the inferior GIMP process, and display it.

Return the GIMP image number.

Does what you mean(TM) in `dired-mode', `image-dired-thumbnail-mode'
and `image-dired-display-image-mode'.

If GIMP is not running as an inferior process, open image(s) with gimp-remote."
  (interactive)
  (let ((img 
	 (case major-mode
	   ((image-dired-thumbnail-mode image-dired-display-image-mode)
	    (get-text-property 
	     (point) 
	     'original-file-name))
	   (dired-mode
	    (with-no-warnings (dired-get-marked-files)))
					; silence compiler, as
					; `dired-get-marked-files' is
					; always present when
					; dired-mode is on
	   (t (read-file-name "File: ")))))
    (if (null img) (error "No image at point"))
    (flet ((open (image)
		   (cond 
                    ((or (gimp-interactive-p) (gimp-cl-p))
                     (let ((command (format
				     "(let  ((image (car (gimp-file-load RUN-INTERACTIVE\n\t%S\n\t%S))))\n\t(car (gimp-display-new image)))\n"
				     (expand-file-name image)
				     (expand-file-name image))))
                       (gimp-insert-and-send-string command)
                       nil))
                    (t 
                     (if (= 0 (shell-command "gimp-remote -q"))
                         (shell-command (format "gimp-remote %s" image))
                       (error "Error: this command requires a running GIMP session."))
		     ))))
      (if (listp img)
	  (mapc 'open img)
	(open img)))))

;; Client Mode
;; Client mode global vars
(defun gimp-make-gimp-file (file)
  (concat gimp-dir "/emacs/" file))

(defvar *gimp-output-file* (gimp-make-gimp-file "emacs-output.scm"))
(defvar *gimp-input-file* (gimp-make-gimp-file "emacs-input.scm"))
(defvar gimp-cl-output "")
(defvar gimp-cl-port nil)
(make-variable-buffer-local 'gimp-cl-port)
(defvar gimp-cl-host nil)
(make-variable-buffer-local 'gimp-cl-host)

(defmacro gimp-with-open-file (filename direction &rest body)
  (declare (indent defun))
  `(with-temp-buffer 
     (case ,direction
       (:input 
        (insert-file-contents-literally ,filename)
       ,@body)
       (:output
        ,@body
        (write-file ,filename nil))
       (:rw
        (insert-file-contents-literally ,filename)
        (prog1 
            (progn ,@body)
          (write-file ,filename nil))))))

(defun gimp-cl-connect ()
  "Connect to the Script-Fu server.

The Script-Fu server is started in the GIMP via Xtns > Script FU
> Start Server."
  (interactive)
  (let ((host (read-from-minibuffer "Host: " "127.0.0.1"))
        (port (read-number "Port: " 10008)))
    (set-process-filter
     (setq gimp-cl-proc
           (open-network-stream "gimp-client"
                                gimp-cl-buffer-name
                                host 
                                port))
     'gimp-cl-process-filter)
    (switch-to-buffer gimp-cl-buffer-name)
    (inferior-gimp-mode)
    (setq gimp-cl-port port
          gimp-cl-host host)
    (insert "Client mode for the GIMP script-fu server\n"
            (make-string 42 61)
            "\n")
    (gimp-restore-caches)
    (gimp-restore-input-ring)
    (gimp-shortcuts)
    (set-marker (process-mark gimp-cl-proc) (point))))

(eval-when (compile load)
  (defun gimp-cl-process-filter (p s)
    (setq gimp-cl-output
          (gimp-with-open-file *gimp-output-file* :input
          (replace-regexp-in-string 
           " +Error: .*$" ;ignore some nonsensical errors (until I
                           ;found out their raison d'être)
           ""
           (buffer-substring-no-properties (point-min)
                                          (point-max)))))))

(defun gimp-cl-send-string (string &optional discard)
  (when (eq (process-status gimp-cl-proc)
            'open)
    (with-no-warnings (gimp-cl-new-output-p))              ;flush any previous output
    (let* ((string (if discard 
                       string
                     (format "(emacs-cl-output %s)" string)))
           (pre "G")
           (len (length string))
           (high (/ len 256))
           (low (mod len 256)))
      (if (> len 65535)
          (error "GIMP send-string: String to long: %d" len))
      (if (> low 0)
          ;; arghh Problems with multibyte and send string. Assert low length of 0
          (setq string (concat string (make-string (- 256 low) ? )) 
                low 0
                high (1+ high)))
      (setq pre (concat pre 
                        (char-to-string high) 
                        (char-to-string low)))
      (if (fboundp 'string-as-unibyte)
          (setq pre (string-as-unibyte pre)))
      (process-send-string gimp-cl-proc pre)
      (process-send-string gimp-cl-proc string))))

(defun gimp-cl-eval-to-string (string &optional discard)
  (gimp-cl-send-string string discard)
  (while (not (with-no-warnings (gimp-cl-new-output-p)))
    (sit-for .1))
  (unless discard
    (if (string-match "^(gimp-error " gimp-cl-output)
        (prog1 (gimp-cl-error gimp-cl-output)
          (gimp-cl-send-string "" nil)) ;flush
      gimp-cl-output)))

(defmacro gimp-cl-error (err)
  err)

(defun gimp-cl-eval (string)
  "Eval STRING, and return it read, somewhat, though not fully, elispified.

Best is to craft STRING so that script-fu returns something universal to the
Lisp world."
  (let ((output (gimp-cl-eval-to-string string)))
    (if (string-match "^#" output)	;unreadable by the lisp reader
        (if (string= "#f\n> " gimp-output) ;gimp returned #f
            nil
          (read (substring output 1)))	;so strip
      (read output))))

(define-derived-mode inferior-cl-gimp-mode inferior-scheme-mode
  "Inferior GIMP"
  "Mode for interaction with inferior gimp process."
  (use-local-map inferior-gimp-mode-map)
  (setq comint-input-filter-functions 
	'(gimp-add-define-to-oblist))
  (add-to-list 'mode-line-process gimp-mode-line-format t))

(defun gimp-send-input ()               
  "Send current input to the GIMP."
  (interactive)
  (let ((gimp-command 
          (cadr (gimp-string-match "^\,\\([[:alpha:]-]+\\)" 
                                  (save-excursion 
                                    (comint-get-old-input-default))))))
    (if gimp-command 
        (set 'gimp-command (intern-soft (concat "gimp-" gimp-command))))
    (cond ((and gimp-command
                (commandp gimp-command))
           (comint-delete-input)
           (let ((input (call-interactively gimp-command)))
             (when (and (eq major-mode 'inferior-gimp-mode)
                        (stringp input))
               (insert input)
               (if (gimp-cl-p)
                   (progn 
                     (insert 
                      (gimp-cl-eval-to-string 
                       (buffer-substring-no-properties
                        (process-mark gimp-cl-proc)
                        (point-max)))
                      (gimp-cl-mkprompt))
                     (set-marker (process-mark gimp-cl-proc) (point)))
                 (gimp-set-comint-filter)
                 (comint-send-input)))))
          (gimp-command (message "No such command: %s" gimp-command))
          (t
	   (let ((undo-list (if (listp buffer-undo-list)
				buffer-undo-list
			      nil)))
	     (setq buffer-undo-list t)  ;Do not record the very
					;verbose tracing in the undo list.
	     (unwind-protect 
		 (progn 
		   (when (get 'gimp-trace 'trace-wanted)
                     (if (gimp-cl-p) 
                         nil            ;tracing does not work in gimp-cl
                       (scheme-send-string "(tracing 1)" t))
		     (sit-for 0.1)
		     (set 'gimp-output ""))
                   (goto-char (point-max))
                   (if (gimp-cl-p)
                       (let ((cl-input (buffer-substring-no-properties
                                     (process-mark gimp-cl-proc)
                                     (point-max))))
                       (mapc (lambda (pv)
                               (apply 'put-text-property
                                      (process-mark gimp-cl-proc)
                                      (point-max)
                                      pv))
                             '((field input)
                               (font-lock-face comint-highlight-input)
                               (front-sticky t)
                               (help-echo "mouse-2: insert after prompt as new input")
                               (mouse-face highlight)))
                       (insert 
                          "\n"
                          (gimp-cl-eval-to-string 
                           cl-input)
                          (gimp-cl-mkprompt))
                         (set-marker (process-mark gimp-cl-proc) (point))
                         (ring-insert comint-input-ring cl-input))
                     (gimp-set-comint-filter)
                     (comint-send-input)))
               (when (get 'gimp-trace 'trace-wanted)
                 (if (gimp-cl-p)  nil ;tracing does not work in gimp-cl
                   (scheme-send-string "(tracing 0)" t))
		 (sit-for 0.1)
		 (set 'gimp-output ""))
	       (setq buffer-undo-list undo-list)))))))

(defun gimp-cl-mkprompt ()
  (propertize "\n> "
              'font-lock-face 'comint-highlight-prompt
              'field 'output
              'inhibit-line-move-field-capture t
              'rear-nonsticky t))

(defun gimp-cl-get-output-time ()
  (nth 5 (file-attributes *gimp-output-file*)))

(eval-when-compile 
  (defun gimp-cl-new-output-p ()))

(eval-when (load run)
  (lexical-let ((old-time (gimp-cl-get-output-time)))
    (defun gimp-cl-new-output-p ()
      (let ((new-time (gimp-cl-get-output-time)))
	(if (not (equal old-time
			new-time))
	    (setq old-time new-time)
	  nil)))))

(defun gimp-cl-p ()
  "Are we hooked into the GIMP as a client?"
  (string= (buffer-name)
           "*Gimp-Client*"))

(provide 'gimp-mode)
;;; gimp-mode.el ends here



