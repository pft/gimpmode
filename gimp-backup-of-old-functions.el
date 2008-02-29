;; FIXME: gimp-eval* funs should be one; for some strange reason, both do not
;; work in all instances.
(defun gimp-eval-be-stringent (string &optional long)
  "Eval STRING, and return it read, somewhat, though not fully, elispified.
Argument LONG with a non-nil value, means a long answer is expected."
  (let (output)
    (if (not (condition-case nil (read (gimp-transvector-g->e string))
	       (error "Error reading input")))
	nil
      (setq gimp-output "")
      (set-process-filter (scheme-proc) 'gimp-filter)
      (scheme-send-string string)
      (unless long (sit-for .1))
      (while
	  (null (setq output
		      (condition-case err (read (gimp-transvector-g->e gimp-output))
			(error nil))))
	(scheme-send-string "" t))
      (if (eq output 'Error:)
          (error "Gimp Error: %s" (substring gimp-output 7 -5))
        output))))

(defun gimp-complete-arg ()
  (interactive)
  (if (and                              ; (looking-at "[ )]*$")
       (>  (gimp-position) 0)
       (member (symbol-name (gimp-fnsym-in-current-sexp)) gimp-pdb-cache))
      (let ((num (gimp-pdb-get-length (gimp-fnsym-in-current-sexp)))
            table)
        (if (>= num (gimp-position))
            (let* ((desc (gimp-describe-function-arg
                          (symbol-name (gimp-fnsym-in-current-sexp))
                          (1- (gimp-position))))
                   (minibuffer-question (format "Value for %s (%s) : " (cadr desc) (car desc))))
              (cond ((string-match "{.*}" (nth 2 desc))
                     (setq table (split-string (nth 2 desc)
                                  "\\(.*{ *\\|, \\|([0-9]+)\\|}.*\\)" t)))
                    ((string-match "(TRUE or FALSE)" (nth 2 desc))
                     (setq table '("TRUE" "FALSE")))
                    ((string-match "The brush name" (nth 2 desc))
                     (setq table (cadr (in-gimp (gimp-brushes-list "")))))
                    ((string-match "The image" (nth 2 desc))
                     (setq table (mapcar 'number-to-string 
                                         (in-gimp (vector->list (cadr (gimp-image-list)))))))
                    ((string-match "file" (cadr desc))
                     (comint-dynamic-complete-filename))
                    ((and (not (gimp-in-string-p))
                          (string-match "STRING" (car desc)))
                     (insert "\"\"")
                     (forward-char -1))
                    (t (gimp-complete-savvy)))
              (when table
                (if (looking-back "[a-zA-Z-]+")
                    (gimp-complete-savvy table)
                  (insert (completing-read minibuffer-question table)))))))
    (gimp-complete-savvy)))


;; (defun gimp-space (n)
;;   (interactive "p")
;;   (if (or (not (looking-back " "))
;; 	  (gimp-in-string-p)) (self-insert-command n))
;;   (gimp-doc)
;;   (if (and (looking-at "[ )]*$")
;; 	   (>  (gimp-position) 0)
;; 	   (member (symbol-name (gimp-fnsym-in-current-sexp)) gimp-pdb-cache))
;;       (let ((num (nth 6 (gimp-describe-function))))
;; 	(if (>= num (gimp-position))
;; 	    (let ((type (car (gimp-describe-function-arg
;; 			      (symbol-name (gimp-fnsym-in-current-sexp))
;; 			      (1- (gimp-position))))))
;; 	      (case (intern-soft (downcase type))
;; 		(string (insert "\"\"")
;; 			(backward-char 1))
;; 	  (float (insert "0."))))))))


(defun gimp-flatten (l)
  (cond ((null l) ())
	((listp l)
	 (append (gimp-flatten (car l)) (gimp-flatten (cdr l))))
	(t (list l))))

;; ;; bluntly adapted from rails-lib.el'
;; (defun gimp-uniq-list (list)
;;   "Return a list of unique elements."
;;   (let ((result '()))
;;     (dolist (elem list)
;;       (when (not (member elem result))
;;         (push elem result)))
;;     result))


;; (defun gimp-doc (&optional sym)
;;   (interactive)
;;   (and scheme-buffer
;;        (get-buffer scheme-buffer)
;;        (comint-check-proc scheme-buffer)
;;        (let ((sym (or sym (symbol-name (gimp-fnsym-in-current-sexp)))))
;;          (if (member sym gimp-pdb-cache)
;;              (let ((resp (gimp-eval
;;                           (format "(emacs-pdb-doc '%s)" sym))))
;;                (setq resp (mapcar (lambda (item) (format "%S" item)) resp))
;;                (setf (car resp)  (propertize sym 'face 'font-lock-keyword-face))
;;                (if  (nth (gimp-position) resp)
;;                    (setf (nth (gimp-position) resp)
;;                          (propertize (nth (gimp-position) resp) 'face 'highlight)))
;;                (setq resp (mapconcat 'identity resp " "))
;;                (message "(%s)" resp))))))

(defun gimp-transvector-g->e (string)
  "Translate Scheme vectors from STRING to Lisp vectors.
Usage: literally read Scheme lists with the Lisp reader.
Caveat: this translates double-quoted scheme vectors too."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "#." nil t)
      (cond ((looking-back "#t\\b")
             (replace-match "t"))
            ((looking-back "#f\\b")
             (replace-match "nil"))
            ((looking-back "(")
             (backward-char 1)
             (let ((pos (1- (point))))
               (forward-sexp 1)
               (delete-char -1)
               (insert ?\])
               (goto-char pos)
               (delete-char 2)
               (insert ?\[)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gimp-transvector-e->g (string)
  "Translate Scheme vectors from STRING to Lisp vectors.
Usage: literally read Scheme lists with the Lisp reader.
Caveat: this translates double-quoted scheme vectors too."
  (let ((re "\\[\\(.*\\)\\]" ))
    (while (string-match re string)
      (setq string
            (replace-regexp-in-string re
                                      (lambda (m)
                                        (format "#(%s)"
                                          (substring m 1 -1))) string  nil nil)))
    string))

(defun gimp-call-procedure-at-point ()
  "Call procedure at point.
This command fails on procedures that take arguments.
Argument ARG is value of first argument."
  (interactive)
  (let ((proc (gimp-procedure-at-point)))
    (when proc
      (gimp-eval-to-string
       (format "(%S 1)" proc)))))

(defun gimp-complete-symbol (&optional predicate)
  "Perform completion on script-fu symbol preceding point.
Compare that symbol against the known script-fu symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered."
  (interactive)
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
      (let* ((lijst gimp-oblist-cache)
             (end (point))
	     (beg (with-syntax-table emacs-lisp-mode-syntax-table
		    (save-excursion
                      (backward-sexp 1)
		      (while (= (char-syntax (following-char)) ?\')
			(forward-char 1))
		      (point))))
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


(defun gimp-describe-function-arg (fun arg)
  (interactive)
  (let  ((output (gimp-eval
		  (format "(gimp-procedural-db-proc-arg \"%s\" %d)" fun arg))))
    (cons (gimp-type-to-readable-type (car output))
	  (cdr output))))

(defvar gimp-type-to-readable-type-map
  '((0  . "INT32")
    (1  . "INT16")
    (2  . "INT8")
    (3  . "FLOAT")
    (4  . "STRING")
    (5  . "INT32ARRAY")
    (6  . "INT16ARRAY")
    (7  . "INT8ARRAY")
    (8  . "FLOATARRAY")
    (9  . "STRINGARRAY")
    (10 . "COLOR")
    (11 . "REGION")
    (12 . "DISPLAY")
    (13 . "IMAGE")
    (14 . "LAYER")
    (15 . "CHANNEL")
    (16 . "DRAWABLE")
    (17 . "SELECTION")
    (18 . "BOUNDARY")
    (19 . "PATH")
    (20 . "STATUS")))

(defun gimp-type-to-readable-type (type)
  (or (cdr (assoc type gimp-type-to-readable-type-map))
      "UNKNOWN"))


;; (defun gimp-complete-or-ask-filename ()
;;   (if (gimp-in-string-p)
;;       (comint-dynamic-complete-filename)
;;     (insert "\"" (read-file-name "File: ") "\"")))

;; (defun gimp-completion-cache-clear (fun pos)
;;   "Clear occurrence of completion candidates for POS argument to FUN in gimp-completion-cache cache"
;;   (aset (gethash fun gimp-completion-cache) pos nil))

;; (defun gimp-completion-cache-clear-all ()
;;   "Clear the entire `gimp-completion-cache'."
;;   (setq gimp-completion-cache nil))
