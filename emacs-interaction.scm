;;; emacs-interaction.scm --- Gimp-side section of Gimp Mode -*- mode: Gimp; -*-

;; Copyright (C) 2008-2009, 2012 Niels Giesen.

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: processes, languages, multimedia, tools

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

;; Stuff needed for interaction with the emacs-modes `gimp-mode'
;; `inferior-gimp-mode' and `gimp-help-mode'.

;; Set the following to #f if you want acces to all (so also
;; uninterned) symbols returned by (oblist); this can be handy to have
;; access to local variables in completion, but also confusing, as a
;; load of symbols will be unbound, and of course slower, because of
;; the huge list generated (2933 versus 1781 symbols in my case).
(define emacs-interaction-possible? #t)

;; Set this to FALSE to export argument names too (clutters
;; completion)
(define emacs-only-bound-symbols? TRUE)

(define emacs-dir 
  (string-append gimp-dir "/emacs/"))

(define (make-emacs-file file)
  (string-append emacs-dir file))

(define-macro (with-output-to-emacs-file  file . form)
  `(with-output-to-file 
       ,(make-emacs-file file)
     (lambda () ,@form)))

(if (not (symbol-bound? 'emacs-first-time?))
    (define emacs-first-time? #t))

(define (emacs-flatten l)
  (cond ((null? l) ())
        ((list? l)
         (append (emacs-flatten (car l)) (emacs-flatten (cdr l))))
        (else (list l))))

(define (emacs-flatten-and-filter-bound l)
 (cond ((null? l) ())
        ((list? l)
         (append (emacs-flatten-and-filter-bound (car l))
                 (emacs-flatten-and-filter-bound (cdr l))))
        ((symbol-bound? l)
         (list l))
        (else ())))

(define (script-fu-dump-for-emacs only-bound? menu? fonts? brushes? patterns? gradients? palettes?)
  
  (when (= menu? TRUE)
        (with-output-to-emacs-file
         "emacs-gimp-menu" ;menu entries for plugins
         (let ((all (gimp-plugins-query "")))
           (write (mapcar (lambda (menu plugin)
                            (list plugin menu))
                          (nth 1 all)
                          (nth 11 all))))))
  (when (= fonts? TRUE)
        (with-output-to-emacs-file                  ;dump fonts list
         "emacs-gimp-fonts-cache"
         (write (cadr (gimp-fonts-get-list "")))))
  (when (= brushes? TRUE)
        (with-output-to-emacs-file                  ;dump brushes list
         "emacs-gimp-brushes-cache"
         (write (cadr (gimp-brushes-get-list "")))))
  (when (= patterns? TRUE)
        (with-output-to-emacs-file                  ;dump patterns list
         "emacs-gimp-patterns-cache"
         (write (cadr (gimp-patterns-get-list "")))))
  (when (= gradients? TRUE)
        (with-output-to-emacs-file                  ;dump gradients list
         "emacs-gimp-gradients-cache"
         (write (cadr (gimp-gradients-get-list "")))))
  (when (= palettes? TRUE)
        (with-output-to-emacs-file                  ;dump patterns list
         "emacs-gimp-palettes-cache"
         (write (cadr (gimp-palettes-get-list "")))))
  (with-output-to-emacs-file                  ;dump oblist
   "emacs-gimp-oblist-cache"
   (write (if (= only-bound? TRUE)
	      (emacs-flatten-and-filter-bound (oblist))
              (emacs-flatten (oblist)))))
  (gimp-procedural-db-dump              ;dump the dump
   (string-append gimp-dir "/dump.db")))


(script-fu-register "script-fu-dump-for-emacs"
		    (if (>= (string->number (substring (car (gimp-version)) 0 3)) 2.5)
			 "<Image>/Filters/Languages/Script-Fu/Dump internals for Emacs' Gimp Mode..."
			 "<Toolbox>/Xtns/Languages/Script-Fu/Dump internals for Emacs' Gimp Mode...")
                    _"Dump (part of) the oblist, fonts, the menu structure and
the procedural database more for use with Emacs' Gimp Mode which you can find at
http://niels.kicks-ass.org/gimpmode.

After you have run this script, you should run M-x gimp-restore-caches from
within Emacs, for the changes to take effect.

If running Gimp as an inferior process from within Emacs (this is possible at
least on Linux) can also run this script from within emacs, via M-x
gimp-dump-for-emacs. To totally refresh everything it is easiest to run M-x
gimp-refresh-all: this updates the Gimp's and Gimp Mode's knowledge on all
scripts and symbols.

Note that this script gets run with defaults arguments on GIMP start-up, if
already present in gimp-dir, so most of the time, you will not need to run
this. However, it may come in handy at the time of registration (or for
debugging)."

                    "Niels Giesen (niels.giesen@gmail.com)"
                    "Niels Giesen"
                    "2008-05-17"
                    ""
                    SF-TOGGLE	_"Only dump bound symbols?" TRUE
		    SF-TOGGLE	_"Dump Menu Structure?"	TRUE
                    SF-TOGGLE	_"Dump fonts?"   	TRUE
		    SF-TOGGLE	_"Dump brushes?"	TRUE
		    SF-TOGGLE	_"Dump patterns?"	TRUE
		    SF-TOGGLE	_"Dump gradients?"	TRUE
		    SF-TOGGLE	_"Dump palettes"	TRUE)
                                                             
;; Fix error hook; ToDo: file bug-report (original has (apply (pop-handler)) 
;; instead of (apply (pop-handler) x))
(define *error-hook* (lambda x (if (more-handlers?) (apply (pop-handler) x) (apply error x))))

(define *emacs-cl-output* nil)
;; Evals once, loads evaluated expression into image, writes output of
;; evaluation back, overwrites *error-hook*. Does not solve (or:
;; introduces) write, display and read problems. 
(define-macro (emacs-cl-output . body)
    (let ((input-file "emacs-input.scm")
          (output-file "emacs-output.scm"))
      (unless (memq gimp-cl-handler *handlers*)
              (push-handler gimp-cl-handler))
      `(begin 
         (with-output-to-emacs-file ,input-file
             (write 
              '(set! *emacs-cl-output*  ,@body))
	     (newline)
             (write '(with-output-to-emacs-file
                         ,output-file
                         (write *emacs-cl-output*)))
	     (newline))
         (load ,(make-emacs-file input-file)))))

(define gimp-cl-handler 
  (lambda x
    (with-output-to-emacs-file
        "emacs-output.scm"
        (apply error x)
	(newline))
    (*error-hook* x)))

;; Dump stuff only on start-up
(if emacs-first-time?
    (begin
      (script-fu-dump-for-emacs emacs-only-bound-symbols? TRUE TRUE TRUE TRUE TRUE TRUE)
      (set! emacs-first-time? #f)))

