;;; fud.scm --- Scheme-side section of Fud -*- mode: Gimp; -*-

;; Copyright (C) 2008-2009, 2012 Niels Giesen.

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: processes, languages, multimedia, tools

;; Author: Niels Giesen <nielsforkgiesen@gmailspooncom, but please
;; replace the kitchen utensils with a dot before hitting "Send">
;; Keywords: processes, multimedia, extensions, tools, gimp, scheme
;; Homepage: http://niels.kicks-ass.org/gimpmode
;; Keywords: lisp, tools, scheme, debugging

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

;; FUD stands for the FU Debugger. It is developed for use
;; with gimp-mode to debug code written in the TinyScheme
;; implementation shipped with the GIMP, but its design is such that
;; it could in principle work with any scheme.
;; 
;; It's basis is setting and handling break-points FUD.scm integrates
;; with fud.el, which in turn is integrated in gimp-mode.

(define (fud-prompt)
  (newline)
  (fud-write-string "FUD> "))

(define fud-result #f) 

(define fud-recursion 0)

(define (fud-recursion++)
  (set! fud-recursion 
	(+ 1 fud-recursion)))

(define (fud-recursion--)
  (set! fud-recursion 
	(- fud-recursion 1)))

(define (fud-reset)
  (set! fud-recursion 0))

(define (fud-write-string . strngs)
  (display 
   (string-append 
    (make-string fud-recursion)
    (unbreakupstr strngs ""))))

(define fud-tracing #t)

(define (fud-untrace)
  (set! fud-tracing #f))

(define (fud-trace)
  (set! fud-tracing #t))

(define (fud-inside-steppable? form)
  (list? form))

(define-macro (fud-log . form) 
  "Very simple logging."
  `(let ((evalled (eval (car ',form))))
     (newline)
     (display "FUD log on")
     (display evalled)
     (newline)
     evalled))

(define-macro (unfud f)
;;  "Remove instruction by fudify on function F"
  (let
    ((name f))
    `(begin
       (if
	(not
	 (assq ',name fudlist))
	(error
	 (string-append
	  (symbol->string ',f)
	  " is not fudified")))
       (define ,f
	 (eval
	  (cadr
	   (assq ',name fudlist))))
       (set! fudlist
	     (fud-delete
	      (assq ',name fudlist)
	      fudlist))
       ',name)))

(define (fud-breakify sxp)
  (list 'fud-break "0.0 file:nil" sxp))

(define-macro (fud-break breakpoint . form)
  `(begin
     (when fud-tracing
	   (newline))
     (fud-recursion++)
     (when fud-tracing
	   (fud-write-string
	    (string-append "Break I-> " ,breakpoint))
	   (newline)
	   (fud-write-string "I: ")
	   (write (car ',form)))
     
     (newline)
     (fud-write-string
      "ENTER: step over, "
      (if (fud-inside-steppable? (car ',form))
	  " I: step inside" "")
      " Q: quit,"
      " G: go"
      " P: poke at environment,"
      " V: use value")
     (fud-prompt)
     (prog1 
      (call/cc 
       (lambda (return)
	 (let (($ #f))

	   ;; INPUT
	   (case (char-downcase (read-char))
	      ;; Go...
	      ((#\g)
	       (display "Continued... (fud-trace) to trace again")
	       (newline)
	       (return (eval (car ',form))))
	      ;; Inside...
	      ((#\i)
	       (read-char)
	       (if (fud-inside-steppable? (car ',form))
		   (set! $ (eval (fud-instruct-1 (car ',form))))))
	      ;; Quit...
	      ((#\q)
	       (fud-reset)
	       (*error-hook* "Quit tracing"))
	      ;; Inspect...
	      ((#\p)
	       (letrec ((handler (lambda (err)
				   (display err) "")))
		 (display "Expression (q to quit inspection): ")
		 (fud-prompt)
		 (let loop ((expr (read)))
		   (cond ((eqv? 'q expr))
			 ((push-handler handler)
			  (fud-write-string "")
			  (write (eval expr))
			  (if (and (pair? *handlers*)
				   (eq? handler (car *handlers*))) 
			      (pop-handler))
			  (newline)
			  (fud-write-string "Type expression: ")
			  (fud-prompt)
			  (loop (read)))))))
	      ;; Use value...
	      ((#\v)
	       (read-char)
	       (fud-write-string "Enter value: ")
	       (fud-prompt)
	       (set! $ (eval (read)))
	       (read-char)))

	 ;; OUTPUT
	 (let (($ (or $ (eval (car ',form)))))
	   (when fud-tracing
		 (fud-write-string

		  (string-append "Break O<- " ,breakpoint))
		  
		 (display " on ")
		 (write (car ',form))
		 (newline)
		 (fud-write-string "O: ")
		 (write $)
		 
		 (newline)
		 (fud-write-string
		  "ENTER: next breakpoint, "
		  " Q: quit,"
		  " G: go (skipping breakpoints),"
		  " P: poke at environment,"
		  " V: use value")
		 (fud-prompt)

		 (case (char-downcase (read-char)) 
		    ;; Go...
		    ((#\g)
		     (display "Continued... (fud-trace) to trace again")
		     (newline)
		     (fud-untrace))
		    ;; Quit...
		    ((#\q)
		     (fud-reset)
		     (*error-hook* (make-environment)))
		    ;; Inspect...
		    ((#\p)
		     (letrec ((handler (lambda (err)
					 (display err) "")))
		       (display "Expression (q to quit inspection) current value is bound to `$': ")
		       (fud-prompt)
		       (let loop ((expr (read)))
			 (cond ((eqv? 'q expr))
			       ((push-handler handler)
				(fud-write-string "")
				(write (eval expr))
				(if (and (pair? *handlers*)
					 (eq? handler (car *handlers*))) 
				    (pop-handler))
				(newline)
				(fud-write-string "Type expression: ")
				(fud-prompt)
				(loop (read)))))))
		    ;; Use value...
		    ((#\v)
		     (fud-write-string "Enter value: ")
		     (fud-prompt)
		     (return (eval (read))))
		    (else (return $)))
		 (return $))))))
      (fud-recursion--)
      (if (= fud-recursion 0)
	  (fud-trace)))))

(define (fud-instruct-1 thunk)
 ;;  "Instruct evaluatable members of THUNK with `fud-breakify'.

;; Special forms and macros supported are if, cond, let, let*, letrec, do
;; and lambda. Forms BEGINNING with a symbol in `blacklist' are returned
;; as is. 

;; For instruction of functions and macros --blacklisted here--, see
;; `fudify' and `unfud'."
  (let ((in-let? #f)
	(in-lambda? #f)
	(in-do? #f)
	(in-cond? #f)
	(blacklist '(define define-macro))
	(blacklisted? #f)
	(num 0))
    (if (eq? (car thunk) 'quote)
	(cdr thunk))
    (mapcar (lambda (th)
	      (set! num (+ 1 num))
	      (cond 
	       ;; IF
	       ((and (= num 1)
		     (memq th '(if)))
		th)
	       ;; COND
	       ((and (= num 1)
		     (memq th '(cond)))
		(set! in-cond? #t)
		'cond)
	       (in-cond? 
		(mapcar (lambda (clause)
			  (fud-breakify clause)) th))
	       ;; LET, LET* and LETREC
	       ((and (= num 1)
		     (memq th '(let let* letrec)))
		(set! in-let? #t)
		th)
	       (in-let?
		(if (symbol? th) th
		    (begin
		      (set! in-let? #f)
		      (mapcar (lambda (th)
				(list (car th)
				      (fud-breakify (cadr th)))) th))))
	       ;; DO
	       ((and (= num 1)
		     (memq th '(do)))
		(set! in-do? 'bindings)
		th)
	       ((eq? in-do? 'bindings)
		(if (symbol? th) th
		    (begin
		      (set! in-do? 'test)
		      (mapcar (lambda (th)
				(cond ((= (length th) 2)
				       (list (car th)
					     (fud-breakify (cadr th))))
				      ((= (length th) 3)
				       (list (car th)
					     (fud-breakify (cadr th))
					     (fud-breakify (caddr th))))))
			      th))))
	       ((eq? in-do? 'test)
		(set! in-do? #f)
		(list (car th)
		      (fud-breakify (cadr th))))
	       ;; LAMBDA
	       ((and (= num 1)
		     (memq th '(lambda)))
		(set! in-lambda? #t)
		'lambda)
	       (in-lambda?	       
		(set! in-lambda? #f)
		th)
	       ;; BLACKLISTED FORMS
	       ((or blacklisted?
		    (and (= num 1)
			 (memq th blacklist)))
		(set! blacklisted? #t)
		th)
	       (else
		(fud-breakify th))))
	    thunk)))

;; Instruction of functions:
(define (fud-delete item lst)
  (let
      ((out
	'()))
    (map
     (lambda
	 (item-in-list)
       (if
	(not
	 (equal? item item-in-list))
	(set! out
	      (cons item-in-list out))))
     lst)
    (if
     (pair? out)
     (reverse out))))

(define fudlist
  '())

(define-macro (fudify f)
;; "Instruct a function for debugging;

;; Any subsequent call to function F will cause fud-breaks to occur at
;; any immediate sublevel of F.  Remove the instruction with (unfud
;; FUNTION).

;; Any redefinition will uninstruct the function. This will give you the
;; error `Function is already fudified' if after that you want to fudify
;; it again.  Client agents can to some intercepting to make this
;; automatic."
  (let
      ((name f))
    `(begin
       (if
	(assq ',name fudlist)
	(error
	 (string-append
	  (symbol->string ',f)
	  " is already fudified")))
       (set! fudlist
	     (cons
	      (list ',name
		    (get-closure-code ,f))
	      fudlist))
       (define ,f
	 (eval
	  (fud-instruct-1
	   (get-closure-code ,f)))))))


