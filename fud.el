;;; fud.el --- Fu Debugger - elisp side

;; Copyright (C) 2008-2009, 2012  Niels Giesen

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: lisp, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; FUD stands for the FU Debugger. It is the elisp side for fud.scm It
;; is developed for use with gimp-mode to debug code written in the
;; TinyScheme implementation shipped with the GIMP, but its design is
;; such that it could in principle work with any scheme.
;; 
;; Its basis is setting break-points.  It can also parse the output
;; of fud.scm, and provides 

;; `fud-set-breakpoint'      set a breakpoint around a sexp.
;; `fud-remove-breakpoint'   remove breakpoint at point
;; `fud-update-breakpoints'  update breakpoints in sexp before point

;; These functions handle output from fud.scm:

;; `fud-reference-in-string' return sexp decribing a breakpoint
;; `fud-find-reference'      lookup the code belonging to
;; `fud-prettify-code'       prettify the output
;; `fud-echo-value'          echo the (in- or outvalue) of the code in breakpoint

;;; Code:
(require 'cl)
(defgroup fud nil
  "Customization group for the FUD Fu Debugger."
  :group 'gimp)

(defface fud-bullet-face
  '((t (:foreground "#d00"
        :bold t)))
  "Face for menu items."
  :group 'fud)

(define-fringe-bitmap 'fud-bullet 
  [0 60 126 -1 -1 -1 126 60 0])

(set-fringe-bitmap-face 'fud-bullet 'fud-bullet-face)

(defun fud-show-breakpoint-bullet (&optional pos)
  (put-text-property
   (or pos (point))
   (+ 1
      (or pos (point)))
   'display
   '(left-fringe fud-bullet)))
   
(defun fud-toplevel (&optional arg)
  (interactive "p")
  (end-of-defun arg))

(defgroup fud nil
  "FUD Unified Debugger"
  :group 'gimp)

(defvar fud-map 
  (let ((m (make-sparse-keymap)))
    (define-key m [(down-mouse-3)]
      (lambda ()
	"Put point where mouse is, and do what ENTER does at that spot."
	(interactive)
	(let ((event (read-event)))
	  (mouse-set-point event)
	  (when (= 41 (char-after))
	    (forward-char 1)
	    (backward-sexp 1))
 	  (fud-remove-breakpoint))))
    m))

(defun fud-field (str)
  (propertize
    str
    'field 'breakpoint
    'font-lock-face 'fud-bullet-face
    'help-echo (format "Breakpoint (toggle with <mouse-3>)\n%s" str)
    'intangible t
    'mouse-face 'highlight
    'rear-nonsticky '(mouse-face font-lock-face)
    'keymap fud-map))

(defun fud-breakpoint-begin ()
  (save-excursion
   (fud-field 
    (concat
     "("
     (propertize
      (format 
       "fud-break \"%d.%d [[ file:%s ]]\""
       (line-number-at-pos)
       (- (point)
	  (point-at-bol))
       (buffer-file-name))
      'display
      '(left-fringe fud-bullet))))))

(defun fud-breakpoint-end ()
  (fud-field ")"))

(defun fud-set-breakpoint ()
  "Insert breakpoint around sexp following point."
  (interactive)
  (insert (fud-breakpoint-begin))
  (forward-sexp 1)
  (insert (fud-breakpoint-end))
(save-excursion 
    (fud-toplevel)
    (gimp-send-last-sexp)))

(defun fud-mouse-set-breakpoint ()
  "Set a fud breakpoint with the mouse."
  (interactive)
  (let ((event (read-event)))
    (mouse-set-point event)
    (fud-set-breakpoint)))

(defun fud-remove-breakpoint ()
  "Remove breakpoint a point."
  (interactive)
  (progn
    (while
	(not 
	 (eq (cadr (member 'field (text-properties-at (point))))
	     'breakpoint))
      (forward-char -1))
    (forward-char)
    (delete-backward-char 
     (- (field-end (point))
	(field-beginning (point))))
    (forward-sexp)
    (forward-char 1)
    (delete-backward-char
     (- (field-end (point))
	(field-beginning (point)))))
  (save-excursion 
    (fud-toplevel)
    (gimp-send-last-sexp)))

(defun fud-update-breakpoints (&optional beg end)
  "Update breakpoints for sexp before point."
  (interactive)
  (condition-case err
      (save-excursion
	(let ((pos (or end 
		       (and mark-active
			    (region-end))
		       (point)))
	      (beg (or beg 
		       (and mark-active
			    (region-beginning)))))
	  (if beg (goto-char beg)
	    (backward-list 1))
	  (while (re-search-forward "(fud-break[[:space:]]+\"[0-9]+.[0-9]+ \\[\\[ file:.+ \\]\\]\"" pos t)
	    (replace-match 
	     (fud-breakpoint-begin))
	    (forward-sexp 1)
	    (insert (fud-breakpoint-end))
	    (delete-char 1))))
    (error "Not at list")))

(defvar fud-reference-re "Break \\(I->\\|O<-\\) \\([0-9]+\\)\\.\\([0-9]+\\) \\[\\[ file:\\(.+\\) \\]\\]")

(defun fud-reference-in-string (str)
  "Return (LINE COLUMN FILENAME IN/OUT) for FUD reference in STR.
If not found, return nil."
  (if (string-match fud-reference-re str)
      (values (string-to-number (match-string 2 str))
	      (string-to-number (match-string 3 str))
	      (match-string 4 str)
	      (string= "O<-" (match-string 1 str)))
    nil))

(defun fud-find-reference (str)
  (multiple-value-bind (line column file-name out-p)
      (fud-reference-in-string str)
    (when file-name 			;when entered at REPL, just stay there ;)
      (if (< (length (window-list)) 2)
	  (split-window))
      (other-window 1)
      (switch-to-buffer (fud-file-name-buffer file-name))
      (when 
	  line
	(goto-line line))
      (when column 
	(beginning-of-line)
	(forward-char column))
      (forward-sexp 1)
      (if (not out-p)
	  (forward-sexp -1))
      (other-window 1))))

(defun fud-prettify-code (str)
  (with-temp-buffer
    (insert str)
    (scheme-mode)
    (pp-buffer)
    (indent-region (point-min)
		   (point-max))
    (buffer-string)))

(defun fud-echo-value (str)
  (if (string-match "^[[:space:]]*[IO]:.*$" str)
      (progn (message (match-string 0 str))
	     t)))

(defun fud-file-name-buffer (file-name &optional blist)
  (or 
   (let* ((blist (or blist (buffer-list)))
	  (buffer (car blist)))
     (cond 
      ((null (cdr blist)) nil)
      ((string= (buffer-file-name buffer) file-name)
       buffer)
      (t (fud-file-name-buffer file-name (cdr blist)))))
   ;; Open file if necessary:
   (if (file-exists-p file-name)
       (find-file file-name))))

(defun fud-run-tests ()
  (interactive)
  (assert 
   (fud-reference-in-string "Break O<- 241.58 [[ file:/home/sharik/.emacs.d/gimp/fud.scm ]]"))
  (assert 
   (not (null (nth 3 (fud-reference-in-string "Break O<- 241.58 [[ file:/home/sharik/.emacs.d/gimp/fud.scm ]]")))))
  (assert 
   (fud-reference-in-string "Break I-> 241.58 [[ file:/home/sharik/.emacs.d/gimp/fud.scm ]]"))
  (assert 
   (fud-reference-in-string "Break I-> 241.58 [[ file:nil ]]"))
  (assert (fud-echo-value " I: (spaces->underscores \"13091q wekjq wejkoiqp wejqwe qwek op123\")"))
  (assert (fud-echo-value "O: (spaces->underscores \"13091q wekjq wejkoiqp wejqwe qwek op123\")"))
  (message "All tests passed"))

(provide 'fud)
;;; fud.el ends here



