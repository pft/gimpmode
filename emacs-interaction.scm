;; -*- mode: Gimp; -*-
;; Stuff needed for interaction with the emacs-modes `gimp-mode'
;; `inferior-gimp-mode' and `gimp-help-mode'.

;; Set the following to #f if you want acces to all (so also uninterned) symbols
;; returned by (oblist); this can be handy to have access to local variables in
;; completion, but also confusing, as nothing is bound, and of course slower,
;; because of the huge list generated (2933 versus 1781 symbols in my case).
(define emacs-only-bound-symbols? TRUE)

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

(define (script-fu-dump-for-emacs only-bound? fonts? menu?)
  (when (= menu? TRUE)
    (with-output-to-file
      (string-append gimp-dir "/emacs-gimp-menu") ;menu entries for plugins
   (lambda ()
     (let ((all (gimp-plugins-query "")))
       (write (mapcar (lambda (menu plugin)
                        (list plugin menu))
                      (nth 1 all)
                      (nth 11 all)))))))
  (when (= fonts? TRUE)
    (with-output-to-file                  ;dump fonts list
      (string-append gimp-dir "/emacs-gimp-fonts-cache")
    (lambda ()
      (write (cadr (gimp-fonts-get-list ""))))))
  (with-output-to-file                  ;dump oblist
      (string-append gimp-dir "/emacs-gimp-oblist-cache")
    (lambda ()
      (write (if (= only-bound? TRUE)
                 (emacs-flatten-and-filter-bound (oblist))
                 (emacs-flatten (oblist))))))
  (gimp-procedural-db-dump              ;dump the dump
   (string-append gimp-dir "/dump.db")))
 
(script-fu-register "script-fu-dump-for-emacs"
                    _"Dump internals for Emacs' Gimp Mode"

                    _"Dump (part of) the oblist, fonts, the menu structure and
the procedural database more for use with Emacs' Gimp Mode which you can find at
http://niels.kicks-ass.org/GimpMode.

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
                    SF-TOGGLE	_"Only dump bound symbols?"	TRUE
                    SF-TOGGLE	_"Dump fonts?"	TRUE
                    SF-TOGGLE	_"Dump Menu Structure?"	TRUE)

(script-fu-menu-register "script-fu-dump-for-emacs"
                         _"<Toolbox>/Xtns/Languages/Script-Fu")

(if emacs-first-time?
    (begin
      (script-fu-dump-for-emacs emacs-only-bound-symbols? TRUE TRUE)
      (set! emacs-first-time? #f)))                                   

(define emacs-interaction-possible? #t)
