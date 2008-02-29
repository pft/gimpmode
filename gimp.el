;; See the file README in this directory

;; What does gimp.el stand for? 

;; Hmm, let's try and make up a multiple recursive acronym:

;; (((GNU's Not Unix) Image Manipulation Program)
;;    Interaction
;;    Mode 
;;    Pimped for 
;;    .
;;  ((Emacs May Annihilate Command Structures)
;;   (Language of Insufferable Superfluous Parentheses) (with a period....)))

;; What is it for?

;; gimp.el provides:

;; - inferior-gimp-mode: interaction mode for the Gimp console

;; - TAB-completion on Gimp Procedures, both in inferior gimp buffer and scheme
;; files.  If scheme-complete is provided, that is a fallback for core scheme
;; symbols.

;; - (function (arg1 TYPE).. (argN TYPE)) - echoing a la slime/eldoc
;;  for gimp procedures. Simply require scheme-complete.el by Alex
;;  Shinn if you want something alike for core scheme functions
;;  too.

;; - extensive Gimp Procedural Browser documentation lookup, either full
;; (dedicated buffer) or sparse (echoed)

;; - other documentation

;; - poor mans source code browsing

;; Requirements
(require 'cmuscheme)
(require 'outline)
(eval-when-compile (require 'cl))
;; Global variables

(defvar gimp-output nil
  "Contains output from inferior gimp process.")
(defvar gimp-pdb-cache nil
  "Cache containing all symbols in Gimps Procedural Database.")
(defvar gimp-pdb-desc-cache nil
  "Cache containing descriptions for all symbols in Gimps Procedural Database.")
(defvar gimp-pdb-long-desc-cache nil
  "Cache containing long descriptions (output for `gimp-help-describe') for all symbols in Gimps Procedural Database.")
(defvar gimp-oblist-cache nil
  "Cache containing ALL symbols in TinyFu, whether bound or not.  The last may change.")
(defvar gimp-doc-echo-cache (make-hash-table :test 'equal)
  "Cache for echoes created by `gimp-doc'.")
(defvar gimp-completion-cache (make-hash-table :test 'eql)
  "Completion hash table.")
(defvar gimp-help-history ()
  "History cache for Gimp Help")
(eval-when (compile load) (defconst gimpeldir "~/.gimpel"))

;; High level interactive gimp-functions. These should be kept to a bare minimum, and
;; enough to get one started.
;;;###autoload
(defun run-gimp (no-gui)
  "Start the Gimp and its REPL.
Prefix argument NO-GUI means: do not start the GUI."
  (interactive "P")
  (if (buffer-live-p (get-buffer "*Gimp*"))
      (switch-to-buffer-other-window "*Gimp*")
    (run-scheme (format
		 "gimp %s --batch-interpreter=plug-in-script-fu-eval -b -"
		 (if no-gui "-i" "")))
    (message "Starting-up the Gimp")
    (unwind-protect 
        (while (= (point-max) (point-min))
          (message "%s." (current-message))
          (sit-for .2))
      (setq scheme-buffer (rename-buffer "*Gimp*"))
      (inferior-gimp-mode)
      (if (not (or gimp-pdb-cache       ;no living caches
                   gimp-pdb-desc-cache
                   gimp-oblist-cache))
          (gimp-get-caches))            ;therefore read them from file
      (message "%sloaded" (current-message))
      (scheme-get-process))))

(defalias 'gimp-start 'run-gimp "Alias so people gimp-TABbing can find `run-gimp'")

(defun gimp-quit ()
  (interactive)
  (gimp-eval-to-string "(gimp-quit 0)" t)
  (kill-buffer "*Gimp*")
  (gimp-save-caches)
  (message "Gimp process ended."))

(defun gimp-open (imgs)
  "Open IMGS by the inferior Gimp process, and display it.
IMGS is a list of images.
Return the Gimp image number(s) in a list."
  (interactive (list (cond ((eq major-mode 'dired-mode)
                            (dired-get-marked-files))
                           (t (list (expand-file-name (read-file-name "File: ")))))))
  (mapcar (lambda (image)
            (gimp-eval (format "(let  ((image (car (gimp-file-load RUN-INTERACTIVE %S %S))))
                             (car (gimp-display-new image)))" image image))) imgs))

;; Customization
(defgroup gimp nil "Customization group for Gimp (script-fu) programming."
  :group 'shell
  :group 'scheme
  :group 'multimedia
  :group 'languages)

(defcustom gimp-rel-fu-dir "/usr/share/gimp/2.0/scripts/"
  "Directory of scripts that come with the Gimp. 
The rel in -rel- stands for released."
  :group 'gimp)

(defcustom gimp-src-dir "/home/sharik/src/gimp-2.3.13/"
  "Source directory for the Gimp"
  :group 'gimp)

(defcustom gimp-docs-alist
  '(("script-fu introduction" . "http://www.ve3syb.ca/wiki/doku.php?id=software:sf:start")
    ("yahoo script-fu group" . "http://tech.groups.yahoo.com/group/script-fu/")
    ("mailing lists" . "http://www.gimp.org/mail_lists.html")
    ("developer.gimp.org" . "http://developer.gimp.org/")
    ("registry.gimp.org" . "http://registry.gimp.org/")
    ("local help" . "file:///usr/share/gimp/2.0/help/en/index.html"))
  "Alist of gimp documentation URLs"
  :group 'gimp)

(defconst gimp-interactive t "Provide interaction with inferior gimp process?
Leave this a non-nil-val; this might turn into a defcustom one time or
another.  Now best left at the non-nil value.")

;; Keybindings
(defvar inferior-gimp-mode-map
  (let ((m (copy-keymap inferior-scheme-mode-map)))
    (define-key m "\t" 'gimp-indent-and-complete)
    (define-key m " " 'gimp-space)
    (define-key m "\C-c," 'gimp-describe-this-arg)
    (define-key m "\C-c." 'gimp-doc)
    (define-key m "\C-cf" 'gimp-help-describe)
    (define-key m "\C-ca" 'gimp-help-apropos)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-c?" 'gimp-help-help)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-search)
    m))

(defvar gimp-mode-map
  (let ((m (copy-keymap scheme-mode-map)))
    (define-key m "\t" 'gimp-indent-and-complete)
    (define-key m " " 'gimp-space)
    (define-key m "\C-x\C-e" 'gimp-send-last-sexp)
    (define-key m "\C-c," 'gimp-describe-this-arg)
    (define-key m "\C-c." 'gimp-doc)
    (define-key m "\C-ch" 'gimp-help)
    (define-key m "\C-c?" 'gimp-help-help)
    (define-key m "\C-cf" 'gimp-help-describe)
    (define-key m "\C-ca" 'gimp-help-apropos)
    (define-key m "\C-cd" 'gimp-documentation)
    (define-key m "\C-cs" 'gimp-search)
    m))

(defvar gimp-help-mode-map
  (let ((m (copy-keymap outline-mode-map)))
    (define-key m "\C-m" 'gimp-help-describe-procedure-at-point)
    (define-key m " " 'gimp-space)
    (define-key m "," 'gimp-doc-at-point)
    (define-key m "l" 'gimp-help-last)
    (define-key m "f" 'gimp-help-describe)
    (define-key m "a" 'gimp-help-apropos)
    (define-key m "q" 'bury-buffer)
    (define-key m "?" 'gimp-help-help)
    (define-key m "n" 'next-line)
    (define-key m "p" 'previous-line)
    (define-key m "r" 'run-gimp)
    (define-key m "d" 'gimp-documentation)
    (define-key m "s" 'gimp-search)
    m))

(defun gimp-space (n)
  "Dispatch space to DWIM actions:

- in help (apropos) mode: echo documentation and move to next line
- in other modes:
 - when in a string: insert N spaces
 - otherwise: echo documentation"
  (interactive "p")
  (cond  ((eq major-mode 'gimp-help-mode)
          (gimp-help-describe-procedure-at-point)
          (unless (= (point-at-eol) (point-max)) (next-line)))
         ((or (gimp-in-string-p)
              (and (not (looking-back " ")) (looking-at ")")))
          (self-insert-command n))
         (t (gimp-doc))))

;;From "http://lispy.wordpress.com/2007/12/27/building-a-better-mapcro/":
(defmacro gimp-mapcro (macro &rest args)
  "Mapcar for macros"
  `(progn ,@(apply #'mapcar
                   (lambda (&rest args2)
                     `(,macro ,@args2))
                   args)))

(defmacro gimp-save-cache (c)
  `(with-temp-file ,(format "%s/cache/%s" gimpeldir c)
     (insert (format "%S" ,c))))

(defmacro gimp-get-cache (c)
  `(with-temp-buffer 
     (when (file-exists-p  ,(format "%s/cache/%s" gimpeldir c))
       (find-file ,(format "%s/cache/%s" gimpeldir c))
       (goto-char (point-min))
       (setq ,c (prog1 (read (buffer-substring (point-min) (point-max)))
                  (kill-buffer nil))))))

(defun gimp-save-caches (&optional non-interactive)
;; Caching note: gimp-pdb-cache, gimp-pdb-desc-cache and gimp-oblist-cache get
;; filed. The other ones, as they are hash-tables, do not get cached over sessions.  But
;; the above are the most time consuming anyway. 
  "Write all saveable caches to a file, to later read out again."
  (interactive)
  (if (or non-interactive (yes-or-no-p "Save caches? "))
      (gimp-mapcro gimp-save-cache (gimp-pdb-cache 
                                    gimp-pdb-desc-cache
                                    gimp-oblist-cache))
    (gimp-delete-caches))
  (message "You can save caches anytime by running the command `gimp-save-caches'"))

(defun gimp-delete-caches ()
  (if (yes-or-no-p "Delete old caches from disk? ")
      (mapc (lambda (c)
              (delete-file (format "%s/cache/%s" gimpeldir c)))
            '(gimp-pdb-cache gimp-pdb-desc-cache gimp-oblist-cache))))

(defun gimp-get-caches ()
  (gimp-mapcro gimp-get-cache
               (gimp-pdb-cache 
                gimp-pdb-desc-cache
                gimp-oblist-cache)))

(defvar gimp-pdb-cache nil
  "Cache containing all symbols in Gimps Procedural Database.")
(defvar gimp-pdb-desc-cache nil
  "Cache containing descriptions for all symbols in Gimps Procedural Database.")
(defvar gimp-oblist-cache nil
  "Cache containing ALL symbols in TinyFu, whether bound or not.  The last may change.")
(defvar gimp-doc-echo-cache (make-hash-table :test 'equal)
  "Cache for echoes created by `gimp-doc'.")
(defvar gimp-completion-cache (make-hash-table :test 'eql)
  "Completion hash table.")

(defun gimp-refresh-oblist-cache ()
  "Refresh `gimp-oblist-cache';
This function gets run when calling the command `run-gimp' the first time in an Emacs session."
  (interactive)
  (setq gimp-oblist-cache nil)
  (message "Creating gimp-oblist-cache, bare with us, this takes a little while...")
  (setq gimp-oblist-cache
	(mapcar 'symbol-name
		(gimp-uniq-list!
		 (gimp-eval "(emacs-flatten (oblist))" t))))
  (message "New oblist cache created"))

(defun gimp-refresh-pdb-cache ()
  (interactive)
  (message "Creating gimp-pdb-cache for information on procedural database, please be patient...")
  (setq gimp-pdb-cache
	(gimp-eval
	 "(cadr (gimp-procedural-db-query \".*\" \".*\" \".*\" \".*\" \".*\" \".*\" \".*\"))" t))
  (message "Done"))

;; Modes
(define-derived-mode gimp-mode scheme-mode "Gimp mode" 
  "Mode for editing gimp scripts (script-fu) and interacting with an inferior gimp process."
  (use-local-map gimp-mode-map))

(define-derived-mode inferior-gimp-mode inferior-scheme-mode
  "Inferior Gimp"
  "Mode for interaction with inferior gimp process.

This mode should not be called directly, but be instantiated by the command M-x run-gimp"
  (use-local-map inferior-gimp-mode-map))

(define-derived-mode gimp-help-mode outline-mode "Gimp Help"
  "Help mode for the Gimp. 
Requires running inferior gimp process, see `inferior-gimp-mode'."
  (use-local-map gimp-help-mode-map)
  (setq buffer-read-only t))

;; Core inferior interaction functions
(defun gimp-proc ()
  ;; (unless (or 
  ;;          ;; not a gimp  mode so please leave us alone
  ;;          ;; (not (memq major-mode '(gimp-mode inferior-gimp-mode gimp-help-mode))) 
  ;;          ;; is a gimp mode and gimp is already running
  ;;          (and scheme-buffer
  ;;               (get-buffer scheme-buffer)
  ;;               (comint-check-proc scheme-buffer)))
  ;;   (run-gimp t))
  (scheme-get-process))

(defun gimp-filter (proc string)
  "Filter for inferior gimp-process."
  (setq gimp-output (concat gimp-output string)))

(defadvice comint-send-input (before ungimp activate) 
  "Ungimp process-filter for other scheme processes."
  (if (and (eq (get-buffer-process (current-buffer)) (gimp-proc))
           (gimp-proc)
           (process-filter (gimp-proc))
           'gimp-filter)
    (set-process-filter (gimp-proc) 'comint-output-filter)))

(defun scheme-send-string (string &optional newline)
  "Send STRING to the scheme process.
When optional argument NEWLINE is non-nil, append a newline char."
  (comint-send-string (gimp-proc)
                      (concat string (if newline "\n" ""))))

(defun gimp-send-last-sexp ()
  "Send the previous sexp to the inferior Scheme process."
  (interactive)
  (message "%s"
	   (gimp-eval-to-string
	    (buffer-substring-no-properties
	     (save-excursion (backward-sexp) (point)) (point)))))


;; Evaluation
;; FIXME: Argument LONG needn't be necessary.
(defun gimp-eval (string &optional long)
  "Eval STRING, and return it read, somewhat, though not fully, elispified.
Argument LONG with a non-`nil' value, means a long answer is
expected.  In most cases, LONG can be omitted.  Argument LONG is
used internally in the calls to `gimp-refresh-pdb-cache' and
`gimp-refresh-oblist-cache'. Use it only when you notice that
`gimp-eval' does not return almost instantly."
  (let (output)
    (if (not (condition-case nil (read string)
	       (error nil)))
	nil
      (setq gimp-output "")
      (set-process-filter (gimp-proc) 'gimp-filter)
      (scheme-send-string string t)
      (unless long (sit-for .1))
      (while
	  (null (setq output
		      (condition-case err (if (null (string-match "^#" gimp-output))
					      (read gimp-output)
					    (read (substring gimp-output 1)))
			(error nil))))
	(scheme-send-string "" t))
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

(defmacro in-gimp (body)
  "Evaluate fu sexps. Syntactic sugar.
Optional argument BODY is a scheme sexp.
Caveats:

1. Inclusion of literal Scheme vectors is impossible.  This is due to read
syntax of Emacs lisp.  Be sure to use (vector elem1 elem2...) or the native
Lisp syntax [elem1 ...] for such uses.

2. As body must be a single sexp, use (begin ...) in your scheme code."
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
       (delete  (buffer-substring (point-min) (point-max)) gimp-help-history) 
       (push (buffer-substring (point-min) (point-max)) gimp-help-history)
       (erase-buffer)
       ,@body)
     (goto-char (point-min))))

(defun gimp-documentation ()
  (interactive)
  (let ((doc (completing-read "Documentation: " gimp-docs-alist nil t)))
    (browse-url (cdr (assoc doc gimp-docs-alist)))))

(defun gimp-help-apropos-list (input)
  (loop for i in gimp-pdb-cache when (string-match input i) collect i))

(defun gimp-help-apropos ()
  (interactive)
  (let ((query (read-from-minibuffer "Apropos term: " (thing-at-point 'word))))
    (gimp-help-wrapper
     (insert (mapconcat 'identity (gimp-help-apropos-list query) "\n")))))

(defun gimp-help ()
  (interactive)
  (if (buffer-live-p (get-buffer "*Gimp Help*"))
      (switch-to-buffer (get-buffer "*Gimp Help*"))
    (gimp-help-apropos)))

(defun gimp-help-help ()
  (interactive)
  (case major-mode
    (gimp-help-mode (message "ENTER short, echoed help
SPACE short, echoed, and formal summary
f     full-blown description for procedure at point
i     full-blown description for queried procedure
b     go back to last Gimp Help output
a     apropos: get list of procedures containing queried input (can be regexp)
?     this help"))
    ((gimp-mode inferior-gimp-mode)
     (message 
      "TAB             gimp-indent-and-complete
SPC             gimp-space
C-c ,           gimp-describe-this-arg
C-c .           gimp-doc: echo \"(function (name TYPE)...)\"
C-c a           gimp-help-apropos
C-c f           gimp-help-describe
C-c h           gimp-help
C-c ?           gimp-help-help"))))

(defun gimp-help-last ()
  (interactive)
  (gimp-help-wrapper
   (insert (cadr gimp-help-history))))

(defun gimp-help-describe-procedure-at-point ()
  (interactive)
  (let ((proc (gimp-procedure-at-point)))
    (when proc
      (gimp-echo-procedure-description proc))))

(defun gimp-intern (string-or-symbol)
  (or (intern-soft string-or-symbol)
      (intern string-or-symbol)))

(defun gimp-help-describe ()
  "Describe function in current sexp, or the one at point.
This is a full description, similar to the one in the gimp pdb browser.
The description is shown in the *Gimp Help* buffer.

Use `outline-mode' commands to navigate and fold stuff."
  (interactive)
  (let* ((hist (mapcar (lambda (s) (symbol-name (car s))) gimp-pdb-desc-cache))
         (sym (gimp-intern (or (gimp-procedure-at-point)
                               (car (member (gimp-fnsym-in-current-sexp) gimp-pdb-cache))
                               (completing-read "Procedure: " gimp-pdb-cache nil t
                                                (symbol-name (symbol-at-point))
                                                'hist))))
         (count 0))
    (gimp-help-wrapper
     (insert
      (or (cdr (assoc sym gimp-pdb-long-desc-cache))
          (let ((desc (format
                         "*  %S\n\n%s\n\n%s"
                         sym
                         (gimp-procedure-description sym)
                         (mapconcat (lambda (argument)
                                      (let ((description (car (cddr argument))))
                                        (when (string-match "{.*}" description)
                                          (setq description (replace-regexp-in-string " { " ":\n\n" description))
                                          (setq description (replace-regexp-in-string "}" "" description))
                                          (setq description
                                                (mapconcat
                                                 (lambda (item)
                                                   (if
                                                       (string-match "\\(.+\\) \\(([0-9]+)\\)" item)
                                                       (format "      %-21s%2s"
                                                               (match-string 1 item)
                                                               (match-string 2 item))
                                                     item))
                                                 (split-string description "\\(, \\|\n\\)") "\n")))
                                        (format "** %-2d %-20s %s\n%s"
                                                (incf count)
                                                (cadr argument)
                                                (car argument)
                                                description)))
                                    (gimp-procedure-args sym) "\n\n"))))
              (add-to-list 'gimp-pdb-long-desc-cache (cons sym desc))
              desc))))))

;; General 
(defmacro gimp-without-string (&rest body)
  `(save-excursion
     (if (gimp-in-string-p)
         (gimp-up-string))
     ,@body))

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
  (read (substring (gimp-eval-to-string (format "(emacs-describe-function-args %S)" `',sym)) 1)))

;;Functions to get information on the (lexical and semantical) environment
(defun gimp-procedure-at-point (&optional as-string)
  (let  ((sym (car (member (format "%s" 
                                   ;; Chomping of quotes is needed for gimp-help,
                                   ;; where references to procedures names appear
                                   ;; single-quoted in descriptions
                                   (replace-regexp-in-string "'" "" (symbol-name (symbol-at-point))))
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
  (if (bolp)				;correct, but does not intercept all possible
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

;; Completion
(defun gimp-indent-and-complete ()
  "Indent and complete function or argument at point."
  (interactive)
  (lisp-indent-line t)
  (gimp-complete)
    (gimp-doc))

(defun gimp-complete-savvy (&optional lijst)
  "Perform completion on script-fu symbol preceding point.
Compare that symbol against the known script-fu symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.
Optional argument LIJST specifies a list of completion candidates."
  (interactive)
  ;; First time setup
  (when (null gimp-pdb-cache)
    (gimp-refresh-pdb-cache))
  (when (null gimp-oblist-cache)
    (gimp-refresh-oblist-cache))
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
          (let* ((lijst (or lijst gimp-oblist-cache))
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
                 (predicate
                  (save-excursion
		    (goto-char beg)
		    (if (not (eq (char-before) ?\())
			(lambda (sym)	;why not just nil ?   -sm
			  (or (boundp sym) (fboundp sym)
			      (symbol-plist sym)))
		      ;; Looks like a funcall position.  Let's double check.
		      (if (condition-case nil
			      (progn (up-list -2) (forward-char 1)
				     (eq (char-after) ?\())
			    (error nil))
			  ;; If the first element of the parent list is an open
			  ;; parenthesis we are probably not in a funcall position.
			  ;; Maybe a `let' varlist or something.
			  nil
			;; Else, we assume that a function name is expected.
			'fboundp))))
                 (completion (try-completion pattern lijst nil)))
            (cond ((eq completion t))
                  ((null completion)
                   (message "Can't find completion for \"%s\"" pattern)
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
                     (let ((list (all-completions pattern lijst nil)))
                       (setq list (sort list 'string<))
                       (or (eq predicate 'fboundp)
                           (let (new)
                             (while list
                               (setq new (cons (if (fboundp (intern (car list)))
                                                   (list (car list) " <f>")
                                                 (car list))
                                               new))
                               (setq list (cdr list)))
                             (setq list (nreverse new))))
                       (if (> (length list) 1)
                           (with-output-to-temp-buffer "*Completions*"
                             (display-completion-list list pattern))
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
  (interactive)
  (let ((answer (gethash fun gimp-completion-cache)))
    (if answer (aref answer pos))))

(defun gimp-completion-cache-get-length (fun)
  (length  
   (gimp-completion-cache-arg-vector fun)))

(defun gimp-completion-cache-arg-vector (fun)
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
Caches completion candidates (in the variable `gimp-completion-cache')"
  (interactive)
  (let ((fun (gimp-fnsym-in-current-sexp))
        (pos (gimp-position))
        fun-or-table)
    (if (gimp-completable-at-p)
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
                     (insert (completing-read (car fun-or-table) (cdr fun-or-table)))))
              (let* ((desc (gimp-describe-function-arg
                            fun
                            (1- pos)))
                     (minibuffer-question (format "Value for %s (%s) : " (cadr desc) (car desc))))
                (setq fun-or-table
                      (cond ((string-match "{.*}" (nth 2 desc)) ; doc provided list of values.
                             (split-string (nth 2 desc)
                                           "\\(.*{ *\\|, \\|([0-9]+)\\|}.*\\)" t))
                            ((string-match "run-mode" (nth 1 desc)) (list "RUN-INTERACTIVE" "RUN-NONINTERACTIVE"))
                            ((string-match "FLOAT" (car desc)) nil)        ;You can do your floats yourself
                            ((string-match "(TRUE or FALSE)" (nth 2 desc)) ;doc says: boolean.
                             '("TRUE" "FALSE"))
                            ((string-match "brush" (nth 2 desc)) ;brushes
                             (cadr (in-gimp (gimp-brushes-list ""))))
                            ((string-match "palette" (nth 2 desc)) ;brushes
                             (cadr (in-gimp (gimp-palettes-get-list ""))))
                            ((string-match "image" (nth 2 desc)) ;images
                             (mapcar 'number-to-string
                                     (in-gimp (vector->list (cadr (gimp-image-list))))))
                            ((string-match "file" (cadr desc)) ;files
                             'gimp-file-ask-or-complete)
                            ((and (not (gimp-in-string-p)) ;string
                                  (string-match "STRING" (car desc)))
                             (insert "\"\"")
                             (forward-char -1))
                            (t nil)))
                (when fun-or-table
                  (when (not (functionp fun-or-table))
                    (push minibuffer-question fun-or-table))
                  (gimp-completion-cache-put fun pos fun-or-table)))))
      (gimp-complete-savvy))))

(defun gimp-complete ()
  "Main completion function.
Caches completion candidates (in the variable `gimp-completion-cache')"
  (interactive)
  (let ((fun (gimp-fnsym-in-current-sexp))
        (pos (gimp-position))
        fun-or-table)
    (if (gimp-completable-at-p)
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
                     (insert (completing-read (car fun-or-table) (cdr fun-or-table)))))
              (let* ((desc (gimp-describe-function-arg
                            fun
                            (1- pos)))
                     (minibuffer-question (format "Value for %s (%s) : " (cadr desc) (car desc))))
                (setq fun-or-table
                      (gimp-make-completion desc))
                (when fun-or-table
                  (when (not (functionp fun-or-table))
                    (push minibuffer-question fun-or-table))
                  (gimp-completion-cache-put fun pos fun-or-table)))))
      (gimp-complete-savvy))))

;; (defun gimp-complete ()
;;   "Main completion function.
;; Caches completion candidates (in the variable `gimp-completion-cache')"
;;   (interactive)
;;   (let ((fun (gimp-fnsym-in-current-sexp))
;;         (pos (gimp-position))
;;         fun-or-table)
;;     (if (gimp-completable-at-p)
;;         (progn
;;           ;; (setq fun-or-table (gimp-completion-cache-get fun pos))
;;           ;; (or (and (functionp fun-or-table)
;;           ;;          (funcall fun-or-table))
;;           ;;     (and (listp fun-or-table)
;;           ;;          (not (null fun-or-table))
;;           ;;          (if (or
;;           ;;               (looking-back "[a-zA-Z-]+")
;;           ;;               (gimp-in-string-p))
;;           ;;              (gimp-complete-savvy (cdr fun-or-table))
;;           ;;            (insert (completing-read (car fun-or-table) (cdr fun-or-table)))))
;;               (let* ((desc (gimp-describe-function-arg
;;                             fun
;;                             (1- pos)))
;;                      (minibuffer-question (format "Value for %s (%s) : " (cadr desc) (car desc))))
;;                 (gimp-complete-savvy (gimp-make-completion desc)))))))

;; Helpful echoing
(defun gimp-doc (&optional sym)
  "Echo function  and argument information for SYM.
The echo takes the form of (function (name-1 TYPE)...(name-n TYPE)), where the
argument at point is highlighted."
  (interactive )
  (if (gimp-interactive-p)
      (let* ((sym (or sym (symbol-name (gimp-fnsym-in-current-sexp))))
             (cache-resp (gethash sym gimp-doc-echo-cache))
             (pos (gimp-position)))
        (cond ((member sym gimp-pdb-cache)
	       (unless cache-resp
		 (setq cache-resp (gimp-eval (format "(emacs-pdb-doc '%s)" sym)))
		 (setq cache-resp (mapcar (lambda (item) (format "%S" item)) cache-resp))
		 (setf (car cache-resp)  (propertize sym 'face 'font-lock-keyword-face))
		 (puthash sym cache-resp gimp-doc-echo-cache))
	       (let ((this-arg (nth pos cache-resp)))
		 (when this-arg
		   (when (> pos 0)
		     (setf (nth pos cache-resp)
			   (propertize (nth pos cache-resp) 'face 'highlight)))
		   (message "(%s)" (mapconcat 'identity cache-resp " "))
		   (when (> pos 0)
		     (set-text-properties 0 (length this-arg) nil (nth pos cache-resp))))))
	      ((featurep 'scheme-complete)
	       (message (scheme-get-current-symbol-info)))))))

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
If a procedure, cache the result in `gimp-pdb-desc-cache'"
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

;; Find source of stuff that comes with the Gimp:
(defun gimp-search-fu ()
  "Search for definition of script-fu procedure.
Needs the variable `gimp-rel-fu-dir' to be set."
  (interactive)
  (save-match-data
    (let* ((proc (or (and (string-match "^script-fu-" (symbol-name (gimp-procedure-at-point)))
                          (symbol-name (gimp-procedure-at-point)))
                     (completing-read "Procedure: " gimp-pdb-cache
                                      (lambda (thing) (string-match "^script-fu-" thing)) t)))
           (file (format "%s/%s.scm" gimp-rel-fu-dir (replace-regexp-in-string "^script-fu-" "" proc))))
      (when (and proc (or (file-exists-p file)
                          (setq file
                                (substring
				 (shell-command-to-string
				  (format "grep -m1 -l %s %s/*" proc gimp-rel-fu-dir)
				  ) 0 -1))))
        (find-file file)
        (goto-char (point-min))
        (re-search-forward (format "(define (%s " proc))
        (gimp-mode)
        (backward-sexp 1)
        (gimp-doc)))))

(defun gimp-search-plug-in-file ()
  "Search for a plug-in file.
Needs the variable `gimp-src-dir' to be set."
  (interactive)
  (find-file
   (substring
    (shell-command-to-string (format "grep -r -m1 -l \"#define .*\\\"%s\\\"\" %s*"
                                     (completing-read "Plug-in: " gimp-pdb-cache
                                                      (lambda (thing)
                                                        (string-match "^plug-in-" thing))
                                                      nil (gimp-procedure-at-point t))
                                     (concat gimp-src-dir "/plug-ins/")))
    0 -1)))

(defun gimp-search-core-function ()
  "Search for the definition of core gimp-function.
Needs the variable `gimp-src-dir' to be set."
  (interactive)
  (let ((function
         (replace-regexp-in-string
          "-" "_"
          (completing-read "Function: " gimp-pdb-cache
                           (lambda (thing)
                             (string-match "^gimp-" thing))
			   t  (replace-regexp-in-string "_" "-"
							(symbol-name (symbol-at-point)))))))
    (find-file
     (substring
      (shell-command-to-string (format "grep -r -m1 -l \"^%s\" %s*"
                                       function
				       (concat gimp-src-dir "/libgimp*/")))
      0 -1))
    (goto-char (point-min))
    (re-search-forward (format "^%s" function))
    (beginning-of-line)))

(defun gimp-search (&optional incorrect)
  "Source search dispatch function.
Optional argument INCORRECT is internal only, and given when user inserts a
wrong char at the minibuffer prompt."
  (interactive)
  (let ((choice (read-char (format "%sSearch for (c)ore function. (p)lugin file or (s)cript fu: "
                                   (if incorrect "Incorrect, try again: " "")))))
    (case choice
      ((?c) (gimp-search-core-function))
      ((?p) (gimp-search-plug-in-file))
      ((?s) (gimp-search-fu))
      (t (gimp-search t)))))

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

(setq gimp-completion-rules
'(((lambda (desc name &rest ignore)
     (string-match "file" name))
   .                                    ;files
   (lambda (&rest ignore)
     (comint-dynamic-complete-filename)))
  ("{.*}" 
   .                              ;doc provided list of values.
   (lambda (desc &rest ignore)
     (split-string desc "\\(.*{ *\\|, \\|([0-9]+)\\|}.*\\)" t))) 
  ((lambda (desc name &rest ignore)
     (string-match "run-mode" name))
   .
   ("RUN-INTERACTIVE" "RUN-NONINTERACTIVE"))
  ((lambda (desc name type)
     (string-match "FLOAT" type))
   .
   nil)  ;You can do your floats yourself
  ("(TRUE or FALSE)" . ("TRUE" "FALSE")) ;doc says: boolean.
  ("brush" .                              ;brushes
   (lambda (&rest ignore)
     (cadr (in-gimp (gimp-brushes-list "")))))
  ("palette" .                          ;palettes
   (lambda (&rest ignore)
     (cadr (in-gimp (gimp-palettes-get-list "")))))
  ("image" .                            ;images
   (lambda (&rest ignore)
     (mapcar 'number-to-string
             (in-gimp (vector->list (cadr (gimp-image-list)))))))
  
  ((lambda (desc name type) (and (not (gimp-in-string-p)) ;string
                            (string-match "STRING" type)))
   .
   (lambda (&rest ignore)
     (insert "\"\"")                      ;"side effect"
     (forward-char -1)))
  ("" . nil)))

(provide 'gimp)
;;; gimp.el ends here
