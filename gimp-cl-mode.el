(defvar *gimp-communication-file* (concat gimp-dir "/ecom"))

(defun gimp-cl-connect ()
  (interactive)
  (set-process-filter
   (setq gimp-cl-proc (open-network-stream "gimp-client" "*Gimp-Client*" "127.0.0.1" 10008))
   (lambda (p s)
     (gimp-with-open-file *gimp-communication-file*
       (setq gimp-cl-output
             (buffer-substring-no-properties (point-min)
                                             (point-max))))))
  (switch-to-buffer "*Gimp-Client*")
  (inferior-gimp-mode)
  (insert "Client mode for the GIMP script-fu server\n"
          (make-string 42 61)
          "\n")

  (gimp-shortcuts)
  (set-marker (process-mark gimp-cl-proc) (point)))


(defun gimp-cl-send-string (string &optional discard)
    (let* ((string (if discard 
                       string
                     (format "(emacs-output %s)" string)))
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
      (process-send-string gimp-cl-proc string)))

(defun gimp-cl-eval-to-string (string &optional discard)
  (if (not (gimp-interactive-p))
      "nil"
    (gimp-cl-send-string string discard)
    (while (not (gimp-cl-new-output-p))
      (sit-for .1))                       ;todo: check date of file ecom.
    (unless discard
      gimp-cl-output)))

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
                 (comint-send-input)))))
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
                     (if (gimp-cl-p)
                         (gimp-cl-send-string "(tracing 1)" t)
                         (scheme-send-string "(tracing 1)" t))
		     (sit-for 0.1)
		     (set 'gimp-output ""))
                   (if (gimp-cl-p)
                       (progn 
                         (goto-char (point-max))
                         (insert 
                          "\n"
                          (gimp-cl-eval-to-string 
                           (buffer-substring-no-properties
                            (process-mark gimp-cl-proc)
                            (point-max)))
                          (gimp-cl-mkprompt))
                         (set-marker (process-mark gimp-cl-proc) (point)))
                   (call-interactively 'comint-send-input)))
	       (when (get 'gimp-trace 'trace-wanted)
                 (if (gimp-cl-p)
                     (gimp-cl-send-string "(tracing 0)" t)
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
  (nth 5 (file-attributes *gimp-communication-file*)))

(lexical-let ((old-time (gimp-cl-get-output-time)))
  (defun gimp-cl-new-output-p ()
    (let ((new-time (gimp-cl-get-output-time)))
      (if (not (equal old-time
                      new-time))
          (setq old-time new-time)
        nil))))

(defun gimp-cl-p ()
  "Are we hooked into the GIMP as a client?"
  (string= (buffer-name)
           "*Gimp-Client*"))

(gimp-cl-new-output-p)

;; Format output back as a string
;; ' (gimp-cl-send-string
;;    "(define (int--emacs-tostring item)
;;       (cond 
;;         ((vector? item) 
;;          (string-append \"[\" (unbreakupstr
;;             (map int--emacs-tostring (vector->list item)) \" \") \"]\")
;;         ((number? item) (number->string item 10))
;;         ((string? item) (string-append \"\\\"\" item  \"\\\"\" ))
;;         ((null? item)  \"()\")
;;         ((eq? #t item) \"t\")
;;         ((pair? item)
;;          (string-append \"(\" (unbreakupstr
;;             (map int--emacs-tostring item) \" \") \")\"))
;;         (TRUE (symbol->string item))))")

;; (gimp-cl-send-string "(with-output-to-file
;;                       (string-append gimp-dir \"/ecom\") (lambda () (write \"1j23io123\")))")

;; (gimp-cl-send-string "(get-closure-code emacs-outputs)")

;; (gimp-cl-send-string  "(define-macro (emacs-outputs . body)
;;   `(with-output-to-file 
;;        (string-append gimp-dir \"/ecom\")
;;      (lambda ()
;;        (write ,@body)
;;        (write \"\n\> \")
;; )))")





;; ' (defun gimp-cl-send-string (string)
;;     (let* ((pre "G")
;;            (len (length string))
;;            (high (/ len 256))
;;            (low (mod len 256)))
;;       (if (> len 65535)
;;           (error "GIMP send-string: String to long: %d" len))
;;       (if (> low 0)
;;           ;; arghh Problems with multibyte and send string. Assert low length of 0
;;           (setq string (concat string (make-string (- 256 low) ? )) 
;;                 low 0
;;                 high (1+ high)))
;;       (setq pre (concat pre 
;;                         (char-to-string high) 
;;                         (char-to-string low)))
;;       ;; (message "to GIMP: %d %d %S %S %s" low high pre (string-make-unibyte pre) string)
;;       (if (fboundp 'string-as-unibyte)
;;           (setq pre (string-as-unibyte pre)))
;;       (process-send-string gimp-cl-proc pre)
;;       (process-send-string gimp-cl-proc string)))


(defmacro gimp-with-open-file (filename &rest body)
  (declare (indent defun))
  `(with-temp-buffer 
     (insert-file-contents-literally ,filename)
     ,@body))

