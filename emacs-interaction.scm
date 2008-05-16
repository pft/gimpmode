;; -*- mode: Gimp; -*-
;; Stuff needed for interaction with the emacs-modes `gimp-mode'
;; `inferior-gimp-mode' and `gimp-help-mode'.

;; Set the following to #f if you want acces to all (so also uninterned) symbols
;; returned by (oblist); this can be handy to have access to local variables in
;; completion, but also confusing, as nothing is bound, and of course slower,
;; because of the huge list generated (2933 versus 1781 symbols in my case).
(define emacs-only-bound-symbols? #t)                                    

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

(define (emacs-cache)
  (with-output-to-file
      (string-append gimp-dir "/emacs-gimp-menu") ;menu entries for plugins
   (lambda ()
     (let ((all (gimp-plugins-query "")))
       (write (mapcar (lambda (menu plugin)
                        (list plugin menu))
                      (nth 1 all)
                      (nth 11 all))))))
  (with-output-to-file                  ;dump fonts list
      (string-append gimp-dir "/emacs-gimp-fonts-cache")
    (lambda ()
      (write (cadr (gimp-fonts-get-list "")))))
  (with-output-to-file                  ;dump oblist
      (string-append gimp-dir "/emacs-gimp-oblist-cache")
    (lambda ()
      (write (if emacs-only-bound-symbols?
                 (emacs-flatten-and-filter-bound (oblist))
                 (emacs-flatten (oblist))))))
  (gimp-procedural-db-dump              ;dump the dump
   (string-append gimp-dir "/dump.db")))

(emacs-cache)

(define emacs-interaction-possible? #t)