;; -*- mode: gimp; -*-
;; Copyright (C) 2008  Niels Giesen

;; Author: Niels Giesen <sharik@localhost>
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

;; FUD stands for the FU Unified Debugger.
;; 
;; It's basis is handling break-points set interactively through
;; another breakpoint, or by use of fud.el with Emacs, which is
;; integrated in gimp-mode.

;; (define fud-last-result #f)

;; (define-macro (fud . x)
;;   (fud-trace)
;;   `(prog1 
;;     (call/cc
;;      (lambda (exit)
;;        (push-handler (lambda (e)
;; 		       (exit e)))
;;        ,@x))
;;     (fud-trace)))

;; (define-macro (fudr)
;;   (fud-trace)
  
;;   `(prog1 
;;     (display "FUD> ")
;;     (let 
;; 	((result 
;; 	  (call/cc
;; 	   (lambda (exit)
;; 	     (push-handler (lambda (e)
;; 			     (display e)
;; 			     (exit e)))
;; 	     (let ((res (eval (read))))
;; 	       (print res)
;; 	       res)))))
;; ;;    (pop-handler)
;; ;;      (display result)
;;     (unless (eq? result 'fud-quit)
;; 	      (fudr)))))

;; (define (fud-repl)
;;   (newline)
;;   (catch (fud-repl)
;; 	 (display "FUD> ")
;; 	 (print (eval (fud(read))))
;; 	 (fud-repl)))

(define (fud-prompt)
  (newline)
  (fud-write-string "FUD> "))

;; (define (fud-handler x)
;;   (set! fud-last-result x)
;;   (display (string-append "Error: " x))
;;   (newline)
;;   (display "Use value (V) / Abort (A) ")
;;   (let ((c (read-char)))
;;     (cond ((char-ci=? c #\v)
;; 	   (push-handler fud-handler)
;; 	   (read))
;; 	  ((char-ci=? c #\a)
;; 	   (throw x)))))

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
	      ((#\s)
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

(define (fud-breakify sxp)
  "Return SXP embedded in a FUD breakpoint instruction."
  (list 'fud-break "0.0 file:nil" sxp))

(define (fud-instruct-1 thunk)
  "Instruct evaluatable members of THUNK with `fud-breakify'.

Special forms and macros supported are if, cond, let, let*, letrec, do
and lambda. Forms BEGINNING with a symbol in `blacklist' are returned
as is. 

FOr instruction of functions, see `fudify' and `unfud'."
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
(define (delete item lst)
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
"Instruct a function for debugging;

Any subsequent call to function F will cause fud-breaks to occur at
any immediate sublevel of F.  Remove the instruction with (unfud
FUNTION).

Any redefinition will uninstruct the function. This will give you the
error `Function is already fudified' if after that you want to fudify
it again.  Client agents can to some intercepting to make this
automatic."
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

(define-macro (unfud f)
  "Remove instruction by fudify on function F"
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
	     (delete
	      (assq ',name fudlist)
	      fudlist))
       ',name)))






































































;; (define-macro (fud . x)
;;   (let ((fudilicious #t))
;;   `(set! fud-last-result
;; 	 (call/cc (lambda (fudilicious)
;; 		    ,@x)))))

;; (define-macro (fud . x)
;;   (fud-trace)
;;   (catch (delay fud-last-result)
;;   `(call/cc (lambda (k)
;; 	      (push-handler (lambda (x)
;; 			      (set! fud-last-result x)
;; 			      (throw x)))
;; 		,@x))))

;; 	  ((char-ci=? c #\i)
;; 	   (display "Expression (quit to quit) : ")
;; 	   (let loop ((var (read)))
;; 	     (push-handler fud-handler)
;; 	     (cond ((eqv? 'quit var)
;; 		    (throw x))
;; 		   ((catch "threw an error" (display (eval var)))
;; 		    (newline)
;; 		    (display "Expression: ")
;; 		    (loop (read))))))

;; (define-macro (fud . x)
;;   `(begin
;;      (fud-trace)
;;      (push-handler (lambda (x)
;; 		     (set! fud-last-result x)
;; 		     (throw x)))
;;      (set! fud-result 
;; 	   (catch (delay fud-last-result)
;; 		  ,@x))))


;; (define-macro condition-case
;;    (lambda (form)
;;      (let ((label (gensym))
;; 	   (err (gensym)))
;;        `(call/cc
;; 	 (lambda (exit)
;; 	   (push-handler
;; 	    (lambda (,err)
;; 	      (exit (apply ,(cadr form) ,err))))
;; 	   (let ((,label (begin ,@(cddr form))))
;; 	     (pop-handler)
;; 	     ,label))))))
