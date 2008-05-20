;;; gimp-mode.el --- $Id: gimp-mode.el,v 1.14 2008-05-20 06:23:08 sharik Exp $
;; Copyright (C) 2008  Niels Giesen <(rot13 "avryf.tvrfra@tznvy.pbz")>


;; Author: Niels Giesen <(rot13 "avryf.tvrfra@tznvy.pbz")>
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
(eval-when-compile (load "gimp-init.el"))
(eval-when (compile load)
  (require 'ring)
  (require 'snippet)
  (require 'scheme-complete))
 ;; Structure
(defgroup gimp nil "Customization group for Gimp (script-fu) programming."
  :group 'shell
  :group 'scheme
  :group 'multimedia
  :group 'languages)
(defgroup gimp-faces 
  nil
  "Customization group for Gimp Mode faces"
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
  "Face for procedures that have been visited with Gimp Help"
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
(defconst gimp-user-generated-caches
  '(gimp-completion-cache)
  "User generated caches (will be saved on quit)")
(defconst gimp-interactive t
  "Provide interaction with inferior gimp process?
Leave this a non-nil-val; this might turn into a defcustom one time or
another.  Now best left at the non-nil value.")
(defvar gimp-current-procedure nil 
  "Value of queried procedure in Gimp Help buffer")
(make-variable-buffer-local 'comint-input-filter-functions)
(make-variable-buffer-local 'gimp-current-procedure)
(defvar gimp-output nil
  "Contains output from inferior gimp process.")
;; (Bases of following caches) generated on Gimp startup (by
;; emacs-interaction.scm)
(defvar gimp-dump nil 
  "Dump of pdb")
(defvar gimp-pdb-cache nil
  "Cache containing all symbols in Gimps Procedural Database.")
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
(defvar gimp-shortcuts
  '("help" "apropos" "describe-procedure" "menu" "documentation"
    "search" "selector" "trace" "untrace" "shortcuts" "open-image"
    "load-script" "refresh-scripts" "gimp-version" "gimp-mode-version"
    "report-bug" "list-snippets" "quit")
  "Commands that can be completed from inferior Gimp buffer.")
(defvar gimp-completion-cache (make-hash-table :test 'eql)
  "Completion hash table.")
(defvar gimp-help-ring ())
(setf (get 'gimp-help-ring 'gimp-help-positions)
      (make-vector 100 1))
(put 'gimp-help-ring 'index 0)
 ;; Customization
(defgroup gimp-directories nil
  "Directories where the Gimp finds its sources"
  :group 'gimp)

(defcustom gimp-src-dir (expand-file-name "~/src/gimp-2.4/")
  "Source directory for the Gimp.

On Debian(-derivatives), get the source for your distribution with apt-get
source gimp"
  :group 'gimp-directories
  :type 'string)

(defcustom gimp-dir (expand-file-name "~/.gimp-2.4")
  "Fall-back user directory for the Gimp.

Setting this variable is only necessary for non-interactive use,
such as on windhoos.

The Gimp puts its caches here.  Retrieve it by evaluating the variable
`gimp-dir' in Gimp script-fu console."
  :group 'gimp-directories
  :type 'string)

(defcustom gimp-data-dir "/usr/share/gimp/2.0/"
  "Fall-back data dir of the Gimp.

Setting this variable is only necessary for non-interactive use,
such as on windhoos.

Value returned by evaluating the variable `gimp-dir' in the Gimp
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
    ("gimp talk" . "http://www.gimptalk.com/forum/"))
  "Alist of gimp documentation URLs"
  :group 'gimp
  :type '(alist :key-type string :value-type string))

(defcustom gimp-program-command-line "gimp -c --batch-interpreter=plug-in-script-fu-eval -b -"
  "Arguments to give to the Gimp. 

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

(defcustom gimp-cache-always nil
  "When non-nil gimp-mode saves caches at end of a session without asking."
  :group 'gimp
  :type 'boolean)

(defcustom gimp-inhibit-start-up-message nil
  "Inhibit start-up message for the Gimp"
  :group 'gimp
  :type 'boolean)

 ;; General purpose functions and macros
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
 ;; Keybindings
(defvar inferior-gimp-mode-map
  (let ((m (copy-keymap inferior-scheme-mode-map)))
    (define-key m "\t" 'gimp-indent-and-complete)
    (define-key m " " 'gimp-space)
    (define-key m "\C-c," 'gimp-describe-this-arg)
    (define-key m "\C-c." 'gimp-doc)
    (define-key m "\C-cf" 'gimp-describe-procedure)
    (define-key m "\C-ca" 'gimp-apropos)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-search)
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
    (define-key m "\C-c." 'gimp-doc)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-cf" 'gimp-describe-procedure)
    (define-key m "\C-ca" 'gimp-apropos)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-search)
    (define-key m "\C-cm" 'gimp-menu)
    (define-key m "\C-ci" 'run-gimp)
    (define-key m "\C-cr" 'gimp-toggle-fuzzy-completion)
    (define-key m "\M-\\" 'comint-dynamic-complete-filename)
    m))

(defvar gimp-help-mode-map
  (let ((m (copy-keymap outline-mode-map)))
    (define-key m "\C-m" 'gimp-help-enter)
    (define-key m "\t" 'gimp-hop-fields)
    (define-key m [(backtab)]
      (lambda (n)
        (interactive "p")
        (gimp-hop-fields (- n))))
    (define-key m " " 'gimp-space)
    (define-key m "," 'gimp-doc-at-point)
    (define-key m "f" 'gimp-help-forward)
    (define-key m "b" 'gimp-help-back)
    (define-key m "F" 'gimp-describe-procedure)
    (define-key m "a" 'gimp-apropos)
    (define-key m "q" 'bury-buffer)
    (define-key m "p" 'previous-line)
    (define-key m "n" 'next-line)
    (define-key m "i" 'run-gimp)
    (define-key m "d" 'gimp-documentation)
    (define-key m "s" 'gimp-search)
    (define-key m "m" 'gimp-menu)
    (define-key m "i" 'gimp-insert-sexp-at-repl)
    (define-key m "t" 'outline-toggle-children)
    (define-key m "S" 'gimp-selector)
    (define-key m "R" 'gimp-help-refresh)
    (define-key m "\C-cf" 'gimp-describe-procedure)
    (define-key m "\C-ca" 'gimp-apropos)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-search)
    (define-key m "\C-cm" 'gimp-menu)
    m))

(define-key gimp-help-mode-map
  [(down-mouse-1)]
  (lambda ()
    (interactive)
    (let ((event (read-event)))
      (mouse-set-point event)
      (gimp-help-enter))))

(defun gimp-help-enter (&optional event)
  "Dispatch enter key or mouse click in Gimp Help buffer.
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
	     (?s (gimp-search (symbol-name (gimp-procedure-at-point)))))))
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
          (just-one-space 1)
          (gimp-doc))))
 ;; Set up the modes
(define-derived-mode gimp-mode scheme-mode "Gimp mode" 
  "Mode for editing script-fu and interacting with an inferior gimp process."
  (use-local-map gimp-mode-map)
  (abbrev-mode 1)
  (setq local-abbrev-table gimp-mode-abbrev-table)
  (if (null gimp-oblist-cache)
      (gimp-restore-caches))) 

(define-derived-mode inferior-gimp-mode inferior-scheme-mode
  "Inferior Gimp"
  "Mode for interaction with inferior gimp process."
  (use-local-map inferior-gimp-mode-map)
  (setq comint-input-filter-functions 
	'(gimp-add-define-to-oblist)))
 ;; Versioning
(defun gimp-mode-version ()
  "Version of this mode."
  (interactive)
  (let ((version (gimp-string-match "[1-9]\.[1-9]+" "$Revision: 1.14 $" 0)))
    (if (interactive-p) (message "Gimp mode version: %s" version))
    version))

(defalias 'gimp-gimp-mode-version 'gimp-mode-version)

(defun gimp-gimp-version ()
  "Version of the Gimp."
  (interactive)
  (destructuring-bind (version major minor rev)
      (gimp-string-match
       "\\([[:digit:]]\\)\.\\([[:digit:]]\\)\.\\([[:digit:]]\\)"
       (gimp-eval "(car (gimp-version))"))
    (when (interactive-p)
      (message "Gimp version: %s" version))
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
      (error "Inferior Gimp not running, type M-x run-gimp to start a new session.")))

(defun gimp-filter (proc string)
  "Filter for inferior gimp-process."
  (setq gimp-output
        (replace-regexp-in-string "Eval"  "" (concat gimp-output string))))

(defadvice comint-send-input (before ungimp activate) 
  "Ungimp process-filter for other scheme processes."
  (if (and (eq (get-buffer-process (current-buffer)) 
	       (gimp-proc))
           (gimp-proc)
           (process-filter (gimp-proc))
           'gimp-filter)
      (set-process-filter
       (gimp-proc)
       (lambda (proc string)
         (gimp-filter proc string)
         (comint-output-filter 
          proc
          (replace-regexp-in-string
           "\n> \nEval: (tracing 0)\nEval: tracing\nEval: 0\nApply to: (0)1"
           "" string))))))

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

(defun gimp-send-input ()
  (interactive)
  (let ((gimp-command 
         (cadr (gimp-string-match "^\,\\([[:alpha:]-]+\\)" 
                                  (comint-get-old-input-default)))))
    (if gimp-command 
        (set 'gimp-command (intern-soft (concat "gimp-" gimp-command))))
    (cond ((and gimp-command
                (commandp gimp-command))
           (comint-delete-input)
           (let ((input (call-interactively gimp-command)))
             (when (and (eq major-mode 'inferior-gimp-mode)
                        (stringp input))
               (insert input)
               (comint-send-input))))
          (gimp-command (message "No such command: %s" gimp-command))
          (t
	   (let ((undo-list (if (listp buffer-undo-list)
				buffer-undo-list
			      nil)))
	     (setq buffer-undo-list t) ;Do not record the very
					;verbose tracing in the undo list.
	     
	     (unwind-protect 
		 (progn 
		   (when (get 'gimp-trace 'trace-wanted)
		     (scheme-send-string "(tracing 1)" t)
		     (sit-for 0.1)
		     (set 'gimp-output ""))
		   (call-interactively 'comint-send-input))
	       (when (get 'gimp-trace 'trace-wanted)
		 (scheme-send-string "(tracing 0)" t)
		 (sit-for 0.1)
		 (set 'gimp-output ""))
	       (setq buffer-undo-list undo-list)))))))

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
  (if (not (gimp-interactive-p))
      "nil"
  (setq gimp-output "")
  (set-process-filter (gimp-proc) 'gimp-filter)
  (scheme-send-string string t)
  (unless discard
    (while (not (string-match "^> $" gimp-output)) ;prompt has not yet returned
      (sit-for .01))				   ;keep polling
    (substring gimp-output 0 -2))))                 ;strip prompt

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
     (let ((window (get-window-with-predicate
                    (lambda (w)
                      (eq (window-buffer w)
                          (get-buffer "*Gimp Help*"))))))
       (if window
           (select-window window)
         (when (= (length (window-list)) 1)
           (split-window-vertically (- (window-height)
                                       15)))
         (other-window 1)
         (switch-to-buffer
          "*Gimp Help*")))
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


(defun gimp-help ()
  "Generic Gimp help."
  (interactive)
  (let ((window (get-window-with-predicate
		 (lambda (w)
		   (eq (window-buffer w)
		       (get-buffer "*Gimp Help*"))))))
    (if window
        (select-window window)
      (when (= (length (window-list)) 1)
	(split-window-vertically (- (window-height)
                                    15)))
      (other-window 1)
      (switch-to-buffer
       "*Gimp Help*")
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
                    "For general help on Gimp Mode, please consult the README file.")
            (goto-char (point-min)))))))

(defun gimp-hop-fields (num)
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
        (gimp-hop-fields 1)))))

(defun gimp-menu (&optional submenu)
  "Navigate through Gimp menu structure.
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
  (if (not (gimp-interactive-p))
      (error
       "Inferior Gimp not running, type M-x run-gimp to start a new session."))
  (let ((sexp (thing-at-point 'sexp)))
    (when sexp
      (switch-to-buffer-other-window "*Gimp*")
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
  (interactive)
  (eval (nth 0 gimp-help-ring)))
 ;; Stuff pertaining to running 'n' quitting
;;;###autoload
(defun run-gimp ()
  "Start the Gimp and its REPL."
  (interactive)
  (if (buffer-live-p (get-buffer "*Gimp*"))
      (switch-to-buffer-other-window "*Gimp*")
    (run-scheme gimp-program-command-line)
    (unwind-protect
        (gimp-progress "Starting-up the Gimp "
                       (lambda () (= (point-max) (point-min))))
      (setq scheme-buffer (rename-buffer "*Gimp*"))
      (inferior-gimp-mode)
                                        ;therefore read them from file
      (message "%s The Gimp is loaded. Have FU." (current-message))
      (unless gimp-inhibit-start-up-message
        (gimp-shortcuts t))
      (when (not (gimp-eval
                  "(symbol-bound? 'emacs-interaction-possible?)"))
	(let ((lnk (concat (gimp-dir) "/scripts/" 
			   "99emacs-interaction.scm"))
	      (emacs-interaction.scm
	       (concat gimp-mode-dir "emacs-interaction.scm")))
	  (message "Creating symlink to emacs-interaction.scm...")
	  (make-symbolic-link emacs-interaction.scm lnk t)
	  (gimp-load-script lnk))
	(gimp-progress (concat (current-message)
			       "and loading it ")
		       (lambda ()
			 (not (string-match "> $"  gimp-output)))
		       " done!"))
      (gimp-restore-caches)
      (gimp-restore-input-ring)
      (scheme-get-process))))

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

(defun gimp-quit ()
  "Quit gimp session."
  (interactive)
  (gimp-save-input-ring)
  (gimp-save-caches)
  (gimp-eval-to-string "(gimp-quit 0)" t)
  (kill-buffer "*Gimp*")
  (message "Gimp process ended."))

 ;; Utility functions
(defun gimp-dir ()
  (if (not (gimp-interactive-p))
      gimp-dir
      (gimp-eval "gimp-dir")))

(defun gimp-data-dir ()
  (if (gimp-interactive-p)
      (in-gimp gimp-data-dir)
    gimp-data-dir))

(defun gimp-up-string ()
  "Move point to a place presumable not in a string."
					;(backward-char 1)
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
         (gimp-intern (current-word)))))

(defun gimp-fnsym-in-current-sexp ()
  "Return function symbol in current sexp."
  (let ((p (point)))
    (gimp-without-string
     (gimp-beginning-of-sexp)
     (prog1
	 ;; Don't do anything if current word is inside a string.
         (if (= (or (char-after (1- (point))) 0) ?\")
             nil
           (gimp-current-symbol))
       (goto-char p)))))

(defun gimp-position ()
  "Return position of point in current lambda form."
  (if (bolp)		 ;correct, but does not intercept all possible
                                        ;0-positions, of course
      0
    (gimp-without-string
     (let ((count 0))
       (if (not (looking-at "[ \n\t)\"]+"))
	   (forward-sexp 1))
       (if (looking-back "[ \n\t)]+")
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

(defun gimp-save-caches (&optional non-interactive)
  "Write the description cache to a file, to later read out again.
Optional argument NON-INTERACTIVE forces to save (asks not).

If `gimp-cache-always' is non-nil, save without asking."
  (interactive)
  (if (or gimp-cache-always
          non-interactive
          (y-or-n-p "Save built-up caches (recommended for speed) ? "))
      (progn (mapc 'gimp-save-cache
                   gimp-user-generated-caches)
             (gimp-save-input-ring)
             (message "Caches saved to %s/ (%s)" (gimp-dir)
                      (mapconcat 'symbol-name gimp-user-generated-caches " ")))
    (gimp-delete-caches)))

(defun gimp-delete-caches ()
  (interactive)
  (if (y-or-n-p "Delete the old caches? ")
      (mapc (lambda (cache)
              (let ((file (format "%s/emacs-%S" (gimp-dir) 
                                  cache)))
                (if (file-exists-p file)
                    (delete-file file))
                (if (hash-table-p (symbol-value cache))
                    (clrhash (symbol-value cache))
                  (set cache nil))))
            gimp-user-generated-caches)))

(defun gimp-restore-cache (cache &optional to-hash prefix)
  "Restore cache from disk.
Argument CACHE is the cache to restore.
Optional argument TO-HASH means convert the list to a hash.
Optional argument PREFIX specifies a prefix for the filename."
  (let* ((prefix (or prefix "emacs-"))
         (file (format "%s/%s%s" (gimp-dir) prefix cache)))
    (with-temp-buffer
      (when (file-exists-p file)
        (find-file file)
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
  (message "Reading in caches... ")
  (mapc 'gimp-restore-cache
        '(gimp-menu
          gimp-fonts-cache
	  gimp-brushes-cache
	  gimp-patterns-cache
	  gimp-gradients-cache
	  gimp-palettes-cache))
  (gimp-restore-cache 'gimp-completion-cache t)
  (setq gimp-oblist-cache
        (mapcar 'symbol-name
                (gimp-uniq-list!
                 (gimp-restore-cache
                  'gimp-oblist-cache))))
  (gimp-read-dump)
  (message "%s%s" (current-message) "Done!"))

(defun gimp-read-dump ()
  (let ((file (concat (gimp-dir) "/dump.db"))
        (ht (make-hash-table :test 'eql)))
    (with-temp-buffer
      (find-file file)
      (mapcar (lambda (i)
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
  (interactive)
  (let ((r
         (with-current-buffer "*Gimp*" comint-input-ring)))
    (with-temp-file (format "%s/emacs-%s" (gimp-dir) "comint-input-ring")
      (insert (format "%S" r)))))

(defun gimp-restore-input-ring ()
  (with-current-buffer (get-buffer "*Gimp*")
    (gimp-restore-cache 'comint-input-ring)
    (if (null comint-input-ring)
        (set 'comint-input-ring (make-ring 65)))))

;; Tracing
(defun gimp-trace ()
  "Start tracing FU.

Only for REPL input."
  (interactive)
  (message "Tracing turned on")
  (put 'gimp-trace 'trace-wanted t))

(defun gimp-untrace ()
  "Stop tracing FU.

Only for REPL input."
  (interactive)
  (gimp-eval "(tracing 0)\n")
  (message "Tracing turned off")
  (put 'gimp-trace 'trace-wanted nil))

(define-derived-mode gimp-help-mode outline-mode "Gimp Help"
  "Help mode for the Gimp."
  (use-local-map gimp-help-mode-map)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (if (null gimp-oblist-cache)
      (gimp-restore-caches)))

(defun gimp-documentation ()
  "Shortcut to (online) documentation.

See variable `gimp-docs-alist'"
  (interactive)
  (let ((doc (completing-read "Documentation: " gimp-docs-alist nil t)))
    (browse-url (cdr (assoc doc gimp-docs-alist)))))

(defun gimp-apropos-list (input)
  (loop for i in (sort (mapcar (lambda (l)
                                 (symbol-name (car l)))
                               (gimp-hash-to-list gimp-dump)) 'string<) 
        when (string-match input i) 
        collect i))

(defun gimp-apropos (&optional query)
  "Search pdb for submatch of QUERY."
  (interactive)
  (let* ((query (or query (read-from-minibuffer "Apropos term: " )))
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
			    'gimp-less-shy-face)
		)		       
	      (propertize proc 'mouse-face 'highlight
			  'font-lock-face
			  (if (member (read proc)
				      gimp-help-visited-pages)
			      'gimp-visited-procedure-face
                           'default))))
           (gimp-apropos-list query) "\n")))
    (if (> (length new-contents) 0)
	(gimp-help-wrapper
         `(gimp-apropos ,query)
	 (insert
          (propertize (format "GIMP procedures matching %S\n"
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
  "Select last shown Gimp Help page.

Go back one page in Gimp Help, and set current page to be the last one
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
  "Select last shown Gimp Help page.

Go back one page in Gimp Help, and set current page to be the last one
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

(defun gimp-intern (string-or-symbol)
  (or (intern-soft string-or-symbol)
      (intern string-or-symbol)))

(defun gimp-describe-procedure (&optional proc)
  "Describe function in current sexp, or the one at point.
This is a full description, similar to the one in the gimp pdb browser.
The description is shown in the *Gimp Help* buffer.

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
					 (format "%s %s\n%s"
                                                 (propertize "**" 'font-lock-face 'gimp-shy-face)
                                                 (propertize
                                                  (format "%-2d %-20s %50s"
                                                          (incf count)
                                                          (replace-regexp-in-string "GIMP_PDB_" "" (cadr argument))
                                                          (car argument))
                                                  'font-lock-face
                                                  'gimp-level2-face)
                                                 desc2)))
				     (gimp-get-proc-args sym) "\n\n"))))
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
 (if code (cons '%s (cadr code)) 'nil)))"
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
(defcustom gimp-complete-fuzzy-p nil
  "Perform fuzzy completion?"
  :group 'gimp
  :type 'boolean)

(defcustom gimp-completion-rules

  '(((lambda (desc name type)
       (string-match "FLOAT" type))
     . nil)

    ((lambda (desc name type)
       (string-match "{.*}" name))
     . (lambda (desc &rest ignore)
         (split-string desc "\\(.*{ *\\|, \\|([0-9]+)\\|}.*\\)" t)))

    ((lambda (desc name &rest type)
       (string-match "filename" name))
     . (lambda (&rest ignore)
         (lambda ()
           (interactive)
           (when (not (gimp-in-string-p))
             (insert (format "%S" (expand-file-name "~/")))
             (backward-char 1))
           (comint-dynamic-complete-filename))))

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
       (string-match "font" name))
     . (lambda (&rest ignore)
         gimp-fonts-cache))

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
       (string-match  "pattern" desc))
     . (lambda (&rest ignore)
         gimp-patterns-cache))

    ((lambda (desc name type)
       (string-match "image" name))
     . (lambda (&rest ignore)
	  (in-gimp
	   (mapcar number->string
                  (vector->list (cadr (gimp-image-list)))))))
    
    ((lambda (desc name type)
       (string-match "color" name))
     . (lambda (&rest ignore)
          (if (y-or-n-p "List colors? ")
              (list-colors-display))))
    
    ((lambda (desc name type)
       (string= "operation" name))
     . ("CHANNEL-OP-ADD"
	"CHANNEL-OP-SUBTRACT"
	"CHANNEL-OP-REPLACE"
	"CHANNEL-OP-INTERSECT"))

    ((lambda (desc name type)
       (and (not (gimp-in-string-p))
            (string-match "STRING" type)))
     . (lambda (&rest ignore)
         (insert "\"\"")
         (forward-char -1)))
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
       (string-match "" name))
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
  (condition-case err 
      (progn 
	(gimp-complete)
	(lisp-indent-line t)
	(gimp-doc))
    (error nil)))

(defun gimp-complete-savvy (&optional lst)
  "Perform completion on script-fu symbol preceding point.
Compare that symbol against the known script-fu symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.
Optional argument LST specifies a list of completion candidates."
  (interactive)
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
      (let* ((lst (or lst gimp-oblist-cache))
	     (end (point))
	     (beg (with-syntax-table scheme-mode-syntax-table
		      ;emacs-lisp-mode-syntax-table
		    (if (gimp-in-string-p)
			(save-excursion
			  (re-search-backward "[^\\]\"" nil t)
			  (forward-char 2)
			  (point))
		      (save-excursion
			(backward-sexp 1)
			(while (= (char-syntax (following-char)) ?\')
			  (forward-char 1))
			(point)))))
	     (pattern (buffer-substring-no-properties beg end))
	     ;;
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
		       (setq lst2 (sort lst2 'string<))
					)
		   (if (> (length lst2) 1)
		       (progn 
			 (with-output-to-temp-buffer "*Completions*" 
			   (display-completion-list nil))
					;set up a good buffer (with
					;all them hooks (orig code:)
			 ;; 			 (with-output-to-temp-buffer "*Completions*" 
			 ;; 			   (display-completion-list lst2 completion))
			 (gimp-complete-in-temp-buffer  "*Completions*"
							 completion lst2))
		     
		     ;; Don't leave around a completions buffer that's
		     ;; out of date.
		     (if (not gimp-complete-fuzzy-p)
			 (let ((win (get-buffer-window "*Completions*" 0)))
			   (if win (with-selected-window win (bury-buffer))))
		       (when (= 1 (length lst))
			 (delete-region beg end)
			 (insert (car lst2)))))))
		 (unless minibuf-is-in-use
		   (message "Making completion list...%s" "done"))))))))

(with-completion-in-temp-buffer "*Completions*" "script-fu" gimp-oblist-cache)
(defun with-completion-in-temp-buffer (buffer pattern list)
  (save-window-excursion
    (switch-to-buffer-other-window buffer)
    (let (buffer-read-only)
      (save-excursion
	(erase-buffer)
	(insert 
	 (propertize "Click <mouse-2> on a completion to select it.
In this buffer, type RET to select the completion near point.

Type C-cr to toggle fuzzy completion.
")
	 (mapconcat 
	  (lambda (candidate)
	    (propertize 
	     (replace-regexp-in-string 
	      pattern 
	      (lambda (m)
		(propertize m
			    'font-lock-face
			    'gimp-level2-face))
	      candidate)
	     'mouse-face 'highlight))
	  list
	  "\n"
	  ))))))

(defun gimp-complete-in-temp-buffer (buffer pattern list)
  (save-window-excursion
    (switch-to-buffer-other-window buffer)
    (let (buffer-read-only)
      (save-excursion
	(erase-buffer)
	(insert 
	 (propertize "Click <mouse-2> on a completion to select it.
In this buffer, type RET to select the completion near point.

Type C-cr to toggle fuzzy completion.
")
	 (mapconcat 
	  (lambda (candidate)
	    (propertize 
	     (replace-regexp-in-string 
	      pattern 
	      (lambda (m)
		(propertize m
			    'font-lock-face
			    'gimp-level2-face))
	      candidate)
	     'mouse-face 'highlight))
	  list
	  "\n"
	  ))))))

(defun gimp-completion-cache-put (fun pos vals)
  (interactive)
  (let ((place (gimp-completion-cache-arg-vector fun)))
    (aset place pos vals)))

(defun gimp-completion-cache-get (fun pos)
  "Get completion for argument at POS in FUN."
  (interactive)
  (let ((answer (gethash fun gimp-completion-cache)))
    (if answer (aref answer pos))))

(defun gimp-completion-cache-get-length (fun)
  "Get length of completion vector for FUN."
  (length  
   (gimp-completion-cache-arg-vector fun)))

(defun gimp-completion-cache-arg-vector (fun)
  "Return completion vector for FUN.

Make and cache it if not cached already."
  (or (gethash fun gimp-completion-cache)
      (puthash fun
               (make-vector
                (1+ (length (gimp-get-proc-args fun)))
		nil)
	       gimp-completion-cache)))

(defun gimp-completable-at-p ()
  "Check whether thing at p is completable."
  (let ((fun (gimp-fnsym-in-current-sexp)))
    (and (gethash fun gimp-dump)
	 (> (gimp-completion-cache-get-length fun)
	    (gimp-position)))))

(defun gimp-complete ()
  "Main completion function.

 Important side-effect:
 Caches completion candidates (in the variable `gimp-completion-cache')"
  (interactive)
  (let ((fun (gimp-fnsym-in-current-sexp))
	(pos (gimp-position))
	(gimp-command (save-excursion
			(cadr (gimp-string-match "^,\\([[:alpha:]-]*\\)"
						 (comint-get-old-input-default)))))
	fun-or-table)
    (cond
     (gimp-command (gimp-complete-savvy gimp-shortcuts))
     ((gimp-completable-at-p)
      (progn
	(setq fun-or-table (gimp-completion-cache-get fun pos))
	(or (and (functionp fun-or-table)
		 (funcall fun-or-table))
	    (and (listp fun-or-table)
		 (not (null fun-or-table))
		 (if (or
		      (looking-back "[a-zA-Z-]+")
		      (gimp-in-string-p))
		     (gimp-complete-savvy (cdr fun-or-table))
		   (insert (completing-read (car fun-or-table)
					    (cdr fun-or-table)))))
	    (let* ((desc (gimp-get-proc-arg
                          fun
                          (1- pos)))
		   (gimp-make-completion desc)
                   (minibuffer-question
                    (format "Value for %s (%s, %s) : "
                            (replace-regexp-in-string
                             "^GIMP_PDB_" "" (cadr desc)) (car desc) (caddr desc))))
              (setq fun-or-table
                    (gimp-make-completion desc))
              (when fun-or-table
                (when (not (functionp fun-or-table))
                  (push minibuffer-question fun-or-table))
                (gimp-completion-cache-put fun pos fun-or-table))))))
     (t (gimp-complete-savvy)))))

(defun gimp-make-completion (desc)
  (let ((name (car desc))
        (type (nth 1 desc))
        (desc (nth 2 desc)))
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

(defun gimp-toggle-fuzzy-completion ()
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
	    (try-completion (nth 4 (car completions))
			    (mapcar 'car completions)))
	nil))))
 ;; Shortcuts
(defun gimp-shortcuts (&optional terse)
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
    (error "Not in *Gimp* buffer")))

(defun gimp-make-input-field (arg)
  (propertize arg 'help-echo (gimp-string-match "^\\(.*$\\)" (documentation (intern-soft (format "gimp-%s" arg))) 1)
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

(defun gimp-doc (&optional sym)
  "Echo function  and argument information for SYM.
The echo takes the form of (function (name-1 TYPE)...(name-n TYPE)), where the
argument at point is highlighted.
Optional argument STR"
  (interactive )
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
                             t)         ;break
                         nil))))
                  (t
		   ;; Get it (unless we have it already)
                   (unless cache-resp
                     (setq cache-resp
                           (gimp-get-closure-code sym))
                     (when (not (consp cache-resp))
                       (setq cache-resp nil)))))

            (when cache-resp
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
                                         nil (nth pos cache-resp))))))))))

(defun gimp-doc-at-point ()
  "Call `gimp-doc' on the symbol at point."
  (interactive)
  (gimp-doc (symbol-at-point)))

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
	  (message "%s"
                   (gimp-get-proc-arg-descriptive-name sym (1- (gimp-position)))))
      (message "%S: No information available" sym))))

 ;; Source look-up
(defun gimp-search-fu (proc)
  "Search for definition of script-fu procedure PROC."
  (grep (format "grep -nH \"( *define\\( (\\| +\\)%s\\([^a-z0-9!?<>-]\\|$\\)\" %s/scripts/* %s/scripts/*"
		proc
		(gimp-data-dir)
		(gimp-dir))))

(defun gimp-search-plug-in-file (proc)
  "Search for a plug-in file containing plug-in PROC.
Needs the variable `gimp-src-dir' to be set."
  (grep
   (format "grep -nHri \"#define .*\\\"%s\\\"\" %s*"
           proc
           (concat (gimp-src-dir) "/plug-ins/"))))

(defun gimp-search-core-function (proc)
  "Search for the definition of core procedure PROC.
Needs the variable `gimp-src-dir' to be set."
  (interactive)
  (let ((proc
         (replace-regexp-in-string
          "-" "_" proc)))
    (grep (format "grep -rnH \"^%s[^a-z0-9_]\" %s*"
                  proc
                  (concat (gimp-src-dir) "/libgimp*/")))))

(defun gimp-search (&optional proc)
  "Scavenge source for a procedure."
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
      (internal (gimp-search-core-function proc))
      (extension (gimp-search-plug-in-file proc))
      (plug-in (gimp-search-plug-in-file proc))
      (fu (gimp-search-fu proc))
      (t (error "Look up-for %s unimplemented" proc)))))

 ;; Snippets
(snippet-with-abbrev-table
 'gimp-mode-abbrev-table
 ("reg" . (format 
           "(define (script-fu-$${name})\n$>$.)
 

\(script-fu-register \"script-fu-$${name}\"
                    _\"$${menu-name (use _ before shortcut letter!) }\"
                    _\"$${hint}\"
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

(add-hook 'gimp-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (setq local-abbrev-table gimp-mode-abbrev-table)))

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
 
(defun gimp-list-snippets ()
  "List predefined snippets for GimpMode."
  (interactive)
  (gimp-help-wrapper
   '(gimp-list-snippets)
   (insert
    (propertize "List of predefined Gimp Mode snippets"
                'font-lock-face 'gimp-level1-face)
    "\n(type them in a buffer of script-fu code, and end with ENTER)\n"
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
(defun gimp-selector (char)
  "Gimp buffer switcher similar to `slime-selector.
Argument CHAR is used to choose between buffers.'."
  (interactive "cSwitch to gimp buffer [ilh?]: ")
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
    (?i (call-interactively 'run-gimp))
    (?? (message "i = inferior gimp buffer; l: last lisp buffer; h: help.")
        (sit-for 3)
        (call-interactively 'gimp-selector))
    (t (call-interactively 'gimp-selector))))

(defun gimp-load-script (&optional script)
  "Load a script into the scheme image."
  (interactive)
  (let* ((script (or script (read-file-name "Load script: " (format "%s/scripts/" (gimp-dir)))))
         (command (format "(load \"%s\")" (expand-file-name script))))
    (if (eq this-command 'gimp-send-input)
        command
      (message "%s" (gimp-eval-to-string command)))))

(defun gimp-report-bug (subject)
  "Send a bug report on Gimp Mode."
  (interactive "sGimp Mode Bug Subject: ")
  (let ((from-buffer (current-buffer)))
    (mail nil (rot13 "avryf.tvrfra@tznvy.pbz") 
          subject)

    (insert (format 
             "
This is a bug report for Gimp mode (NOT for the Gimp itself!) .
This bug report will be sent to the Gimp mode maintainer,
not to your local site managers!

Please describe exactly what actions triggered the bug
and the precise symptoms of the bug (it may also be
helpful to include an *EXAMPLE FILE*!):

Gimp-mode version: %s

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

(defun gimp-refresh-scripts ()
  "Refresh script-fu scripts."
  (interactive)
  (let* ((command "(car (script-fu-refresh RUN-INTERACTIVE))"))
    (if (eq this-command 'gimp-send-input)
        command
      (message "Refreshing scripts...")
      (if (gimp-eval command)
          (message "Completed!")
        (message "Failed!")))))

(defun gimp-open-image (image)
  "Open IMAGE by the inferior Gimp process, and display it.

Return the Gimp image number."
  (interactive "fFile: ")
  (if (not (gimp-interactive-p))
      (shell-command (format "gimp-remote %s" image))
    (let ((command (format
		    "(let  ((image (car (gimp-file-load RUN-INTERACTIVE\n\t%S\n\t%S))))\n\t(car (gimp-display-new image)))\n"
		    (expand-file-name image)
		    (expand-file-name image))))
      (if (eq this-command 'gimp-send-input)
	  command
	(comint-send-string (gimp-proc)
			    command)))))

(defun gimp-dump-for-emacs ()
  "Dump stuff for emacs."
  (interactive)
  (let ((command "(car (script-fu-dump-for-emacs TRUE TRUE TRUE TRUE TRUE TRUE))"))
    (if (eq this-command 'gimp-send-input)
        command
      (message "Dumping stuff...")
      (gimp-eval command))))

(defun gimp-refresh-all ()
  "Update both the Gimp's and Gimp Mode's knowledge on all scripts and symbols."
  (interactive)
  (gimp-refresh-scripts)
  (gimp-dump-for-emacs)
  (gimp-restore-caches)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer "*Gimp Help*")
     (gimp-help-refresh))))

(provide 'gimp-mode)
;;; gimp-mode.el ends here

