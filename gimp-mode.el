;;; gimp-mode.el --- $Id: gimp-mode.el,v 1.5 2008-05-10 13:08:18 sharik Exp $
;; Copyright (C) 2008  Niels Giesen <niels.giesen@gmail.com>

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: processes, multimedia, extensions, tools

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

;; Interaction mode for the GIMP; see README for full description and usage.

;;; Code:
(provide 'gimp-mode)


;; Requirements
(require 'cmuscheme)
(require 'outline)
(require 'cl)
(require 'ring)
(eval-when-compile (require 'scheme-complete))

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


;; Gimp Help
(defmacro gimp-help-wrapper (&rest body)
  `(progn
     (unless (eq (current-buffer)
		 (get-buffer "*Gimp Help*"))
       (switch-to-buffer-other-window "*Gimp Help*"))
     (gimp-help-mode)
     (let (buffer-read-only)
       (delete (buffer-substring (point-min) (point-max)) gimp-help-history) 
       (push (buffer-substring (point-min) (point-max)) gimp-help-history)
       (erase-buffer)
       ,@body)
     (goto-char (point-min))))

(defmacro gimp-without-string (&rest body)
  `(save-excursion
     (if (gimp-in-string-p)
         (gimp-up-string))
     ,@body))

;; Version
(defun gimp-gimp-mode-version ()
  "Version of this mode."
  (interactive)
  (let ((version (gimp-string-match "[1-9]\.[1-9]+" "$Revision: 1.5 $" 0)))
    (if (interactive-p) (message "Gimp mode version: %s" version))
    version))

(defun gimp-gimp-version ()
  "Version of the Gimp."
  (interactive)
  (destructuring-bind (version major minor rev)
      (gimp-string-match
       "\\([[:digit:]]\\)\.\\([[:digit:]]\\)\.\\([[:digit:]]\\)"
       (in-gimp (car (gimp-version))))
    (when (interactive-p)
      (message "Gimp version: %s" version))
    (list version major minor rev)))
;; Global variables

(make-variable-buffer-local 'comint-input-filter-functions)
(defvar gimp-output nil
  "Contains output from inferior gimp process.")
;; (Bases of following caches) generated on Gimp startup (by
;; emacs-interaction.scm)
(defvar gimp-pdb-cache nil
  "Cache containing all symbols in Gimps Procedural Database.")
(defvar gimp-fonts-cache nil 
  "Cache of available fonts")
(defvar gimp-oblist-cache nil
  "Cache containing ALL symbols in TinyScheme, whether bound or not.

  The last might be subject to change.")
(defvar gimp-menu nil)
(defvar gimp-shortcuts
  '("help" "apropos" "describe-procedure" "menu" "documentation"
    "search" "selector" "trace" "untrace" "shortcuts" "open-image"
    "load-script" "gimp-version" "gimp-mode-version" "report-bug" "quit")
  "Commands that can be completed from inferior Gimp buffer.")
;; User generated caches (will be saved on quit) :
(defconst gimp-user-generated-caches
  '(gimp-pdb-desc-cache
    gimp-pdb-long-desc-cache
    gimp-completion-cache
    gimp-doc-echo-cache))

(defvar gimp-pdb-desc-cache nil
  "Cache containing descriptions for all symbols in Gimps Procedural Database.")
(defvar gimp-pdb-long-desc-cache nil
  "Cache containing long descriptions (output for `gimp-describe-procedure')\
 for all symbols in Gimps Procedural Database.")
(defvar gimp-doc-echo-cache (make-hash-table :test 'equal)
  "Cache for echoes created by `gimp-doc'.")
(defvar gimp-completion-cache (make-hash-table :test 'eql)
  "Completion hash table.")

(defvar gimp-help-history ()
  "History cache for Gimp Help")

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
  :group 'gimp-mode)

(defcustom gimp-cache-always nil 
  "When non-nil gimp-mode saves caches at end of a session."
  :group 'gimp-mode)

(defcustom gimp-inhibit-start-up-message nil
  "Inhibit start-up message for the Gimp"
  :group 'gimp-mode)

;; Interactive gimp-functions. These should be kept to a bare minimum,
;; and enough to get one started.
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
      (if (not (or gimp-pdb-cache       ;no living caches
                   gimp-pdb-desc-cache
                   gimp-oblist-cache
		   gimp-fonts-cache))
          (gimp-restore-caches))
      (gimp-restore-input-ring)
      (unless gimp-inhibit-start-up-message
        (gimp-shortcuts t))
      (scheme-get-process))))

;; Emacs 22 compatibility
(when (not (fboundp 'ring-member))
  (defun ring-member (ring item)
    "Return index of ITEM if on RING, else nil.
Comparison is done via `equal'.  The index is 0-based."
    (catch 'found
      (dotimes (ind (ring-length ring) nil)
        (when (equal item (ring-ref ring ind))
          (throw 'found ind))))))

(when (not (fboundp 'ring-next))
  (defun ring-next (ring item)
    "Return the next item in the RING, after ITEM.
Raise error if ITEM is not in the RING."
    (let ((curr-index (ring-member ring item)))
      (unless curr-index (error "Item is not in the ring: `%s'" item))
      (ring-ref ring (ring-plus1 curr-index (ring-length ring))))))

(defun gimp-progress (message test)
  (interactive)
  (let ((r (make-ring 4)))
    (mapc (lambda (i)
            (ring-insert r i))
          '(45 47 124 92))
    (message (concat message "/"))
    (while (funcall test)
      (let* ((mess (or (current-message) (concat message "/")))
             (last-char (string-to-char 
                         (substring mess (1- (length mess))
                                    (length mess)))))
        (message "%s%c" (substring mess 0 (1- (length mess))) (ring-next r last-char))
        (sit-for .15)))))

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

(defun gimp-open-image (image)
  "Open IMAGE by the inferior Gimp process, and display it.

Return the Gimp image number."
  (interactive "fFile: ")
  (let ((command (format
                  "(let  ((image (car (gimp-file-load RUN-INTERACTIVE\n\t%S\n\t%S))))\n\t(car (gimp-display-new image)))\n"
                  (expand-file-name image)
                  (expand-file-name image))))
    (if (eq this-command 'gimp-send-input)
        command
      (comint-send-string (gimp-proc)
                          command))))

;; Customization
(defgroup gimp-mode nil "Customization group for Gimp (script-fu) programming."
  :group 'shell
  :group 'scheme
  :group 'multimedia
  :group 'languages)

(defcustom gimp-src-dir (expand-file-name "~/src/gimp-2.4/")
  "Source directory for the Gimp.

On debian(-derivatives), get the source for your distribution with apt-get
source gimp"
  :group 'gimp-mode)

(defcustom gimp-docs-alist
  '(("script-fu introduction" . 
     "http://www.ve3syb.ca/wiki/doku.php?id=software:sf:start")
    ("script-fu tutorial" . "http://docs.gimp.org/en/gimp-using-script-fu-tutorial.html")
    ("yahoo script-fu group" . "http://tech.groups.yahoo.com/group/script-fu/")
    ("mailing lists" . "http://www.gimp.org/mail_lists.html")
    ("developer.gimp.org" . "http://developer.gimp.org/")
    ("registry.gimp.org" . "http://registry.gimp.org/")
    ("local help" . "file:///usr/share/gimp/2.0/help/en/index.html"))
  "Alist of gimp documentation URLs"
  :group 'gimp-mode)

(defconst gimp-interactive t
  "Provide interaction with inferior gimp process?
Leave this a non-nil-val; this might turn into a defcustom one time or
another.  Now best left at the non-nil value.")

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
    (define-key m "\C-m" 'gimp-send-input)
    (define-key m "\M-\\" 'comint-dynamic-complete-filename)
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
    (define-key m "\M-\\" 'comint-dynamic-complete-filename)
    m))

(defvar gimp-help-mode-map
  (let ((m (copy-keymap outline-mode-map)))
    (define-key m "\C-m" 'gimp-describe-procedure-at-point)
    (define-key m " " 'gimp-space)
    (define-key m "," 'gimp-doc-at-point)
    (define-key m "l" 'gimp-help-last)
    (define-key m "f" 'gimp-describe-procedure)
    (define-key m "a" 'gimp-apropos)
    (define-key m "q" 'bury-buffer)
    (define-key m "n" 'next-line)
    (define-key m "p" 'gimp-help-previous-line-and-show)
    (define-key m "r" 'run-gimp)
    (define-key m "d" 'gimp-documentation)
    (define-key m "s" 'gimp-search)
    m))

(define-key gimp-help-mode-map
  [(down-mouse-1)]
  (lambda ()
    (interactive)
    (let ((event (read-event)))
      (mouse-set-point event)
      (cond ((eq (field-at-pos (point)) 'submenu)
             (gimp-menu 
              (gimp-submenu-at-point (point))))
            ((cddr event)
             (gimp-describe-procedure))
            (t (gimp-describe-procedure-at-point))))))

(defun gimp-submenu-at-point (pos)
  "Return submenu at POS."
  (save-excursion 
    (goto-char pos)
    (do ((string (field-string-no-properties (1+ (point)))
                 (concat (field-string-no-properties) "/" string)))
        ((progn (goto-char (1- (field-beginning)))
                (not (eq (field-at-pos (point)) 'submenu))) 
         (concat (replace-regexp-in-string "/*$" "" string) "/")))))

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

(defun gimp-space (n)
  "Dispatch space to DWIM actions:

- in help (apropos) mode: echo documentation and move to next line
- in other modes:
 - when in a string: insert N spaces
 - otherwise: echo documentation"
  (interactive "p")
  (cond  ((eq major-mode 'gimp-help-mode)
          (gimp-describe-procedure-at-point)
          (unless (= (point-at-eol) (point-max)) 
            (call-interactively 'next-line)))
	 (t (self-insert-command n)
	    (gimp-doc))))

(defun gimp-dir ()
  (gimp-eval "gimp-dir"))

(defun gimp-string-match (re str &optional num)
  (when (string-match re str)
    (if num (match-string num str)
      (let (result)
	(dotimes (v (/ (length (match-data)) 2) (reverse result))
	  (push (match-string v str) result))))))

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
Argument CACHE is the cache to restore."
  (let* ((prefix (or prefix "emacs-"))
         (file (format "%s/%s%s" (gimp-dir) prefix cache)))
    (with-temp-buffer
      (when (file-exists-p file)
        (find-file file)
        (goto-char (point-min))
        (set cache (prog1 (read (buffer-substring (point-min) (point-max)))
                     (kill-buffer nil)))
        (when to-hash (set cache (gimp-list-to-hash (symbol-value cache))))))
    (symbol-value cache)))

(defun gimp-restore-caches ()
  "Restore caches from disk.
Normally this function needn't be run interactively, lest a cache has been
screwed up. It is wise then to preceed it with a call to
`gimp-delete-caches'."
  (interactive)
  (mapc 'gimp-restore-cache
        '(gimp-pdb-cache
          gimp-pdb-desc-cache
          gimp-pdb-long-desc-cache
          gimp-menu
          gimp-fonts-cache))
  (mapc (lambda (cache)
          (gimp-restore-cache cache t))
        '(gimp-doc-echo-cache
          gimp-completion-cache))
  (setq gimp-oblist-cache
	(mapcar 'symbol-name
		(gimp-uniq-list!
		 (gimp-restore-cache 'gimp-oblist-cache)))))

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

;; Modes
(define-derived-mode gimp-mode scheme-mode "Gimp mode" 
  "Mode for editing script-fu and interacting with an inferior gimp process."
  (use-local-map gimp-mode-map))

(define-derived-mode inferior-gimp-mode inferior-scheme-mode
  "Inferior Gimp"
  "Mode for interaction with inferior gimp process."
  (use-local-map inferior-gimp-mode-map)
  (setq comint-input-filter-functions 
	'(gimp-add-define-to-oblist)))

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
  "Help mode for the Gimp. 
Requires running inferior gimp process, see `inferior-gimp-mode'."
  (use-local-map gimp-help-mode-map)
  (setq buffer-read-only t))

;; Core inferior interaction functions
(defun gimp-proc ()
  (scheme-get-process))

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
               (set 'gimp-output "")))))))

;; Evaluation
(defun gimp-eval (string)
  "Eval STRING, and return it read, somewhat, though not fully, elispified."
  (let (output)
    (if (not (condition-case nil (read string)
               (error nil)))
        nil
      (setq gimp-output "")
      (set-process-filter (gimp-proc) 'gimp-filter)
      (scheme-send-string string t)
      (sit-for .1)
      (while (null (setq output
        		 (condition-case err
        		     (if (null (string-match "^#" gimp-output))
        			 (read gimp-output)
        		       (read (substring gimp-output 1)))
        		   (error nil))))
        (scheme-send-string "" t))      ;force flush
      output)))

(defun gimp-eval-to-string (string &optional discard)
  (if (not (condition-case nil (read string)
	     (error nil)))
      nil
    (setq gimp-output "")
    (set-process-filter (gimp-proc) 'gimp-filter)
    (scheme-send-string string t)
    (unless discard
      (sit-for .1)
      (substring gimp-output 0 -2))))

(defun gimp-documentation ()
  "Shortcut to (online) documentation.

See variable `gimp-docs-alist'"
  (interactive)
  (let ((doc (completing-read "Documentation: " gimp-docs-alist nil t)))
    (browse-url (cdr (assoc doc gimp-docs-alist)))))

(defun gimp-help ()
  "Generic Gimp help."
  (interactive)
  (if (buffer-live-p (get-buffer "*Gimp Help*"))
      (switch-to-buffer (get-buffer "*Gimp Help*"))
    (gimp-apropos)))

(defun gimp-apropos-list (input)
  (loop for i in (sort (copy-list gimp-pdb-cache) 'string<) 
        when (string-match input i) 
        collect i))

(defun gimp-apropos ()
  "Search pdb for submatch of name."
  (interactive)
  (let* ((query (read-from-minibuffer "Apropos term: " ))
	 (new-contents
          (mapconcat
           (lambda (proc)
             (propertize proc 'mouse-face 'highlight))
           (gimp-apropos-list query) "\n")))
    (if (> (length new-contents) 0)
	(gimp-help-wrapper
	 (insert new-contents))
      (message "No match"))))

(defun gimp-help-last ()
  (interactive)
  (gimp-help-wrapper
   (insert (cadr gimp-help-history))))

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

Use `outline-mode' commands to navigate and fold stuff.

Optional argument PROC is a string identifying a procedure."
  (interactive)
  (let* ((hist
	  (mapcar
	   (lambda (s) (symbol-name (car s)))
	   gimp-pdb-desc-cache))
         (sym
	  (gimp-intern
	   (or
            proc
            (gimp-procedure-at-point)
            (car (member (symbol-name
                          (gimp-fnsym-in-current-sexp)) gimp-pdb-cache))
            (completing-read "Procedure: " gimp-pdb-cache nil t
                             (car (member (symbol-name (symbol-at-point)) gimp-pdb-cache))
                             'hist))))
         (count 0))
    (gimp-help-wrapper
     (insert
      (or (cdr (assoc sym gimp-pdb-long-desc-cache))
          (let ((desc
		 (format
		  "*  %s - %s\n\n%s\n\n%s"
		  (propertize (symbol-name sym) 'mouse-face 'highlight)
                  (let ((menu (cadr (assoc (symbol-name sym) gimp-menu))))
                    (if (null menu)
                        "No menu entry"
                      (setq menu (nreverse (split-string menu "/")))
                      (concat (mapconcat 'identity
                                         (reverse (mapcar (lambda (submenu) ;
                                                            (propertize submenu
                                                                        'mouse-face 'highlight
                                                                        'field 'submenu
                                                                        'help-echo "Find more plugins under this submenu"))
                                                          (cdr menu)))
                                         "/") "/" (car menu))))
		  (gimp-procedure-description sym)
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
		       (format "** %-2d %-20s %s\n%s"
			       (incf count)
			       (cadr argument)
			       (car argument)
			       desc2)))
		   (gimp-procedure-args sym) "\n\n"))))

	    (add-to-list 'gimp-pdb-long-desc-cache (cons sym desc))
	    desc))))))

;; General 
(defun gimp-interactive-p ()
  "Is gimp being run as a subprocess?"
  (and
   gimp-interactive
   scheme-buffer
   (get-buffer scheme-buffer)
   (comint-check-proc scheme-buffer)))

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



;; Internal information retrieval
(defun gimp-describe-function (fun)
  (gimp-eval
   (format "(gimp-procedural-db-proc-info \"%s\")" fun)))
             
(defun gimp-describe-function-arg (fun arg)
  (gimp-eval 
   (format "(emacs-describe-function-arg '%S %d)" fun arg)))

(defun gimp-procedure-description (sym)
  (let ((desc (cdr (assoc sym gimp-pdb-desc-cache))))
    (progn
      (when (not desc)
	(setq desc
	      (gimp-eval
	       (format
		"(emacs-short-description \"%S\")" 
		sym)))
	(add-to-list 'gimp-pdb-desc-cache (cons sym desc)))
      (with-temp-buffer
	(insert desc)
	(fill-region (point-min) (point-max))
	(buffer-substring-no-properties (point-min) (point-max))))))

(defun gimp-procedure-args (sym)
  (read 
   (substring
    (gimp-eval-to-string
     (format "(emacs-describe-function-args %S)" `',sym)) 1)))

;;Functions to get information on the (lexical and semantical) environment
(defun gimp-procedure-at-point (&optional as-string)
  (let  ((sym 
	  (car (member (format "%s" 
;; Chomping of quotes is needed for gimp-help,
;; where references to procedures names appear
;; single-quoted in descriptions
			       (replace-regexp-in-string 
				"'" ""
				(symbol-name (symbol-at-point))))
		       gimp-pdb-cache))))
    (when sym
      (apply 
       (if as-string
           'identity
         'gimp-intern)
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
  (if (bolp)                       ;correct, but does not intercept all possible
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

;; Completion
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
	     (beg (with-syntax-table emacs-lisp-mode-syntax-table
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
	     (completion (try-completion pattern lst nil)))
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
		 (let ((lst2 (all-completions pattern lst nil)))
		   (setq lst2 (sort lst2 'string<))
		   (if (> (length lst2) 1)
		       (with-output-to-temp-buffer "*Completions*"
			 (display-completion-list lst2 pattern))
;; Don't leave around a completions buffer that's
;; out of date.
		     (let ((win (get-buffer-window "*Completions*" 0)))
		       (if win (with-selected-window win (bury-buffer))))))
		 (unless minibuf-is-in-use
		   (message "Making completion list...%s" "done")))))))))

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
                (1+ (nth 6 
			 (gimp-describe-function fun)))
                nil)
               gimp-completion-cache)))

(defun gimp-completable-at-p ()
  "Check whether thing at p is completable."
  (let ((fun (gimp-fnsym-in-current-sexp)))
    (and (member (symbol-name fun) gimp-pdb-cache)
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
            (let* ((desc (gimp-describe-function-arg
                          fun
                          (1- pos)))
                   (minibuffer-question
                    (format "Value for %s (%s) : " (cadr desc) (car desc))))
              (setq fun-or-table
                    (gimp-make-completion desc))
              (when fun-or-table
                (when (not (functionp fun-or-table))
                  (push minibuffer-question fun-or-table))
                (gimp-completion-cache-put fun pos fun-or-table))))))
     (t (gimp-complete-savvy)))))


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
              'field 'input))

;; Helpful echoing
(defun gimp-doc (&optional sym)
  "Echo function  and argument information for SYM.
The echo takes the form of (function (name-1 TYPE)...(name-n TYPE)), where the
argument at point is highlighted."
  (interactive )
  (if (gimp-interactive-p)
      (let ((result))
        (catch 'a
          (let* ((sym (or sym (symbol-name (gimp-fnsym-in-current-sexp))))
                 (cache-resp (gethash sym gimp-doc-echo-cache))
                 (pos (gimp-position)))
            (cond ((member sym gimp-pdb-cache)
;; Get it (unless cached)
                   (unless cache-resp
                     (setq cache-resp
                           (gimp-eval (format "(emacs-pdb-doc '%s)" sym)))))
                  ((unless (string-match "define\\(?:-macro\\)?" sym) 
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
;; Get it (unless cached)
                   (unless cache-resp
                     (setq cache-resp
                           (read
                            (gimp-eval-to-string
                             (apply 'format
                                    "(let ((code (get-closure-code %s)))\
 (if code (cons '%s (cadr code)) 'nil)))"
                                    (make-list 3 sym)))))
                     (when (not (consp cache-resp))
                       (setq cache-resp nil)))))

            (when cache-resp
              (setq cache-resp
                    (mapcar (lambda (item) (format "%s" item))
                            (dotted-to-list cache-resp)))
              (setf (car cache-resp)
                    (propertize sym 'face 'font-lock-keyword-face))
;; Cache it
              (puthash sym cache-resp gimp-doc-echo-cache)
;; Show it
              (let ((this-arg (nth pos cache-resp)))
                (when this-arg
                  (when (> pos 0)
                    (let ((arg (nth pos cache-resp)))
                      (setf (nth pos cache-resp)
                            (if (string-match "^\. " arg)
                                (concat ". " (propertize (substring arg 2) 'face 'highlight))
                              (propertize arg 'face 'highlight)))))
                  (message (format "(%s)%s" (mapconcat 'identity cache-resp " ")
                                   (or result "")))
                  (when (> pos 0)
                    (set-text-properties 0 (length this-arg)
                                         nil (nth pos cache-resp)))))))))))

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

(defun gimp-doc-at-point ()
  "Call `gimp-doc' on the symbol at point."
  (interactive)
  (gimp-doc (symbol-name (symbol-at-point))))

(defun gimp-echo-procedure-description (sym)
  "Echo short description for SYM."
  (message
   (gimp-procedure-description sym)))

(defun gimp-describe-this-arg ()
  "Echo description for argument or procedure at point;
If a procedure, cache the result in `gimp-pdb-desc-cache'."
  (interactive)
  (let* ((sym (gimp-without-string (gimp-fnsym-in-current-sexp))))
    (if (member (symbol-name sym) gimp-pdb-cache)
	(if (eql sym (symbol-at-point))
	    (gimp-echo-procedure-description sym)
	  (message
	   (gimp-eval
	    (format "(list-ref (gimp-procedural-db-proc-arg \"%S\" %d) 2)"
		    sym
		    (1- (gimp-position))))))
      (message "%S: No information available" sym))))

;; Find source of stuff that comes with the Gimp/own scripts:
(defun gimp-search-fu ()
  "Search for definition of script-fu procedure."
  (interactive)
  (save-match-data
    (let* ((proc
	    (or
	     (and
	      (string-match "^script-fu-"
			    (symbol-name (gimp-procedure-at-point)))
	      (symbol-name (gimp-procedure-at-point)))
	     (completing-read "Procedure: " gimp-pdb-cache
			      (lambda (thing)
				(string-match "^script-fu-" thing)) t))))
      (when proc
        (destructuring-bind (v maj min rev)
            (gimp-gimp-version)
          (grep (format "grep -nH \"( *define\\( (\\| +\\)%s\\([^a-z0-9!?<>-]\\|$\\)\" %s/scripts/* ~/.gimp-%s.%s/scripts/*"
                        proc
                        (in-gimp gimp-data-dir)
                        maj
                        min)))))))

(defun gimp-search-plug-in-file ()
  "Search for a plug-in file.
Needs the variable `gimp-src-dir' to be set."
  (interactive)
  (when (or
         (null gimp-src-dir)
         (not (file-exists-p gimp-src-dir)))
    (error "%s does not exist.  Check variable `gimp-src-dir'" gimp-src-dir))
  (grep
   (format "grep -nHri \"#define .*\\\"%s\\\"\" %s*"
           (completing-read "Plug-in: " gimp-pdb-cache
                            (lambda (thing)
                              (string-match "^plug-in-" thing))
                            nil (gimp-procedure-at-point t))
           (concat gimp-src-dir "/plug-ins/"))))

(defun gimp-search-core-function ()
  "Search for the definition of core gimp-fun.
Needs the variable `gimp-src-dir' to be set."
  (interactive)
  (when (or (null gimp-src-dir)
            (not (file-exists-p gimp-src-dir)))
    (error "%s does not exist.  Check variable `gimp-src-dir'" gimp-src-dir))
  (let ((fun
         (replace-regexp-in-string
          "-" "_"
          (completing-read
	   "Function: "
	   gimp-pdb-cache
	   (lambda (thing)
	     (string-match "^gimp-" thing))
	   t
           (let ((fun (symbol-at-point)))
             (and fun
                  (replace-regexp-in-string "_" "-"
                                            (symbol-name fun)) ""))))))
    (grep (format "grep -rnH \"^%s[^a-z0-9_]\" %s*"
                  fun
                  (concat gimp-src-dir "/libgimp*/")))))

(defun gimp-search (&optional incorrect)
  "Source search dispatch function.

Optional argument INCORRECT is internal only.  It is given when user inserts a
wrong char at the minibuffer prompt."
  (interactive)
  (let ((choice
	 (read-char
	  (format "%sSearch for (c)ore function (in C) . (p)lugin file or (s)cript (f)u: "
		  (if incorrect "Incorrect, try again: " "")))))
    (case choice
      ((?c ?C) (gimp-search-core-function))
      ((?p ?P) (gimp-search-plug-in-file))
      ((?s ?f ?S ?F) (gimp-search-fu))
      (t (gimp-search t)))))

(defvar gimp-completion-rules
  '(((lambda (desc name type)
       (string-match "FLOAT" type))
     .
     nil)                               ;You can do your floats yourself
    ((lambda (desc name &rest ignore)
       (string-match "file" name))
     .                                  ;files
     (lambda (&rest ignore)
       'comint-dynamic-complete-filename))
    ("font" . (lambda (&rest ignore)
                gimp-fonts-cache))
    ("The procedure name"
     .
     (lambda (&rest ignore)
       gimp-pdb-cache))
    ("{.*}"
     .                                  ;doc provided list of values.
     (lambda (desc &rest ignore)
       (split-string desc "\\(.*{ *\\|, \\|([0-9]+)\\|}.*\\)" t)))
    ((lambda (desc name &rest ignore)
       (string-match "run-mode" name))
     .
     ("RUN-INTERACTIVE" "RUN-NONINTERACTIVE"))
    ("\\((TRUE or FALSE)\\|toggle\\)" . ("TRUE" "FALSE")) ;; Booleans
    ("brush" .                                            ;brushes
     (lambda (&rest ignore)
       (cadr (in-gimp (gimp-brushes-list "")))))
    ("palette" .			;palettes
     (lambda (&rest ignore)
       (cadr (in-gimp (gimp-palettes-get-list "")))))
    ("image" .                          ;images
     (lambda (&rest ignore)
       (mapcar 'number-to-string
               (in-gimp (vector->list (cadr (gimp-image-list)))))))
    ((lambda (desc name type) (and (not (gimp-in-string-p)) ;string
                              (string-match "STRING" type)))
     .
     (lambda (&rest ignore)
       (insert "\"\"")                  ;"side effect"
       (forward-char -1)))
    ("" . nil)))

(defun gimp-make-completion (desc)
  (let ((desc (caddr desc))
        (name (cadr desc))
        (type (car desc)))
    (let ((action
           (cdr (assoc-if (lambda (rule)
			    (cond ((stringp rule)
				   (string-match rule desc))
				  ((functionp rule)
				   (apply rule (list desc name type)))
				  (t nil))) gimp-completion-rules))))
      (if (functionp action)
          (apply action (list desc name type))
        action))))

;; snippets
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

\(script-fu-menu-register \"$${name}\"
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

(defun gimp-add-define-to-oblist (str)
  "Put vars, functions and macros defined by STR in the oblist."
  (set-text-properties 0 (length str) nil str)
  (let* ((var-or-fun (gimp-string-match
                      "[[:space:]]*(define\\(-macro\\)?[[:space:]]+(?\\([[:word:]-?!><]+\\)" str 2)))
    (if (and var-or-fun (not (member var-or-fun gimp-oblist-cache)))
	(push var-or-fun gimp-oblist-cache))))

(defun gimp-selector (char)
  "Buffer switcher like `slime-selector.
Argument CHAR is used to choose between buffers.'."
  (interactive "cSwitch to gimp buffer [ilh?]: ")
  (case char
    (?l (switch-to-buffer
	 (car
	  (member-if
	   (lambda (b)
	     (with-current-buffer b
	       (eq major-mode 'gimp-mode)))
	   (buffer-list)))))
    (?h (gimp-help))
    (?i (call-interactively 'run-gimp))
    (?? (message "i = inferior gimp buffer; l: last lisp buffer; h: help.")
        (sit-for 3)
        (call-interactively 'gimp-selector))
    (t (call-interactively 'gimp-selector))))

(defun gimp-load-script ()
  "Load a script into the scheme image."
  (interactive)
  (let* ((script (read-file-name "Load script: " (format "%s/scripts/" (gimp-dir))))
         (command (format "(load \"%s\")" (expand-file-name script))))
    (if (eq this-command 'gimp-send-input)
        command
      (comint-send-string (gimp-proc)
                          command))))

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

(defun gimp-menu (&optional submenu)
  "Navigate through Gimp menu structure.
Optional argument SUBMENU defines the default content of the minibuffer."
  (interactive)
  (let* ((entry (completing-read "Select by menu entry: "
                                 (mapcar 'cadr gimp-menu) nil t (or submenu "<")))
         (plug-in (car (rassoc* entry gimp-menu :key 'car :test 'string=))))
    (gimp-describe-procedure plug-in)))

(provide 'gimp-mode)
;;; gimp-mode.el ends here



