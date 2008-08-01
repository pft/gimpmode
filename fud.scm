;; -*- mode: gimp; -*-
(define fud-last-result #f)

;; (define-macro (fud . x)
;;   (fud-trace)
;;   `(call/cc (lambda (k)
;; 					;	      (push-handler fud-handler)
;; 	      ,@x)))

(define-macro (fud . x)
  (fud-trace)
  `(prog1 
    (call/cc
     (lambda (exit)
       (push-handler (lambda (e)
		       (exit e)))
       ,@x))
    (fud-trace)))

(define-macro (fudr)
  (fud-trace)
  
  `(prog1 
    (display "FUD> ")
    (let 
	((result 
	  (call/cc
	   (lambda (exit)
	     (push-handler (lambda (e)
			     (display e)
			     (exit e)))
	     (let ((res (eval (read))))
	       (print res)
	       res)))))
;;    (pop-handler)
;;      (display result)
    (unless (eq? result 'fud-quit)
	      (fudr)))))

(define (fud-repl)
  (newline)
  (catch (fud-repl)
	 (display "FUD> ")
	 (print (eval (fud(read))))
	 (fud-repl)))

(define (fud-prompt)
  (newline)
  (fud-write-string "FUD> "))

(define (fud-handler x)
  (set! fud-last-result x)
  (display (string-append "Error: " x))
  (newline)
  (display "Use value (V) / Abort (A) ")
  (let ((c (read-char)))
    (cond ((char-ci=? c #\v)
	   (push-handler fud-handler)
	   (read))
	  ((char-ci=? c #\a)
	   (throw x)))))

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

;; (define-macro (fud-break line . form)
;;   `(begin
;;      (when fud-tracing
;; 	   (newline))
;;      (fud-recursion++)
;;      (when fud-tracing
;; 	   (fud-write-string
;; 	    (if (number? ,line)
;; 		(string-append "Break at Line "
;; 			       (number->string ,line)
;; 			       ": \n")
;; 		,line))
;; 	   (fud-write-string "I: ")
;; 	   (display (car ',form)))
;;      (prog1
;;       (let (($ (eval (car ',form))))
;; 	(when fud-tracing
;; 	      (newline)
;; 	      (fud-write-string "O: ")
;; 	      (display $)
;; 	      (newline)
;; 	      (fud-write-string
;; 	       "ENTER: go on to next breakpoint, "
;; 	       " Q: quit,"
;; 	       " G: go (skipping breakpoints),"
;; 	       " I: inspect environment,"
;; 	       " V: use value")
;; 	      (fud-prompt)
;; 	      (let ((c (read-char)))
;; 		(cond 
;; 		 ;; Go...
;; 		 ((char-ci=? c #\g)
;; 		  (display "Continued... (fud-trace) to trace again")
;; 		  (newline)
;; 		  (fud-untrace))
;; 		 ;; Quit...
;; 		 ((char-ci=? c #\q)
;; 		  (fud-reset)
;; 		  (*error-hook* (make-environment)))
;; 		 ;; Inspect...
;; 		 ((char-ci=? c #\i)
;; 		  (letrec ((handler (lambda (err)
;; 				      (display err) "")))
;; 		    (display "Expression (q to quit inspection) current value is bound to `$': ")
;; 		    (fud-prompt)
;; 		    (let loop ((expr (read)))
;; 		      (cond ((eqv? 'q expr))
;; 			    (;(catch "threw an error" (display (eval expr)))
;; 			     (push-handler handler)
;; 			     (fud-write-string "")
;; 			     (display (eval expr))
;; 			     (if (and (pair? *handlers*)
;; 				      (eq? handler (car *handlers*))) 
;; 				 (pop-handler))
;; 			     (newline)
;; 			     (fud-write-string "Type expression: ")
;; 			     (fud-prompt)
;; 			     (loop (read)))))))
;; 		 ;; Use value...
;; 		 ((char-ci=? c #\v)
;; 		  (fud-write-string "Enter value: ")
;; 		  (fud-prompt)
;; 		  (set! $ (read))))))
;; 	(fud-recursion--)
;; 	$))))


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
      " Q: quit,"
      " G: go (skipping further breakpoints),"
      " I: inspect environment,"
      " V: use value")
     (fud-prompt)

     (prog1 
      (call/cc 
       (lambda (return)
	 (let ((c (read-char)))
	   (cond 
	    ;; Go...
	    ((char-ci=? c #\g)
	     (display "Continued... (fud-trace) to trace again")
	     (newline)
	     (return (eval (car ',form))))
	    ;; Quit...
	    ((char-ci=? c #\q)
	     (fud-reset)
	     (*error-hook* "Quit tracing"))
	    ;; Inspect...
	    ((char-ci=? c #\i)
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
	    ((char-ci=? c #\v)
	     (fud-write-string "Enter value: ")
	     (fud-prompt)
	     (return (read)))))

	 (let (($ (eval (car ',form))))
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
		  " I: inspect environment,"
		  " V: use value")
		 (fud-prompt)
		 (let ((c (read-char)))
		   (cond 
		    ;; Go...
		    ((char-ci=? c #\g)
		     (display "Continued... (fud-trace) to trace again")
		     (newline)
		     (fud-untrace))
		    ;; Quit...
		    ((char-ci=? c #\q)
		     (fud-reset)
		     (*error-hook* (make-environment)))
		    ;; Inspect...
		    ((char-ci=? c #\i)
		     (letrec ((handler (lambda (err)
					 (display err) "")))
		       (display "Expression (q to quit inspection) current value is bound to `$': ")
		       (fud-prompt)
		       (let loop ((expr (read)))
			 (cond ((eqv? 'q expr))
			       (;(catch "threw an error" (display (eval expr)))
				(push-handler handler)
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
		    ((char-ci=? c #\v)
		     (fud-write-string "Enter value: ")
		     (fud-prompt)
		     (return (read)))
		    (else (return $))))
		 (return $)))))
      (fud-recursion--))))


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