;; This is gimp-init.el

;; Put gimp.el (and scheme-complete.el) somewhere in your `load-path'
;; or add the parent dir of gimp.el to your `load-path'.

(add-to-list 'load-path "~/.emacs.d/elisp")

(autoload 'run-gimp "gimp" 
  "Inferior Gimp Interaction Mode Pimped for Emacs Lispniks" t)

(autoload 'gimp-mode "gimp" 
  "Gimp Interaction Mode Pimped for Emacs Lispniks" t)

(autoload 'gimp-help "gimp" 
  "Help for the Gimp" t)    

;; Uncomment the below if you have scheme-complete.el in your
;; load-path and want to get echoing for core scheme functions too:

;; (add-to-list 'load-path "/path/to/scheme-complete/if/different/from/path/to/gimp.el")
;; (mapc (lambda (hook)
;; 	(add-hook hook
;; 	  (lambda ()
;; 	    (autoload 'eldoc-current-symbol "eldoc")
;; 	    (require 'scheme-complete))))
;;       '(gimp-mode-hook gimp-help-mode-hook inferior-gimp-mode-hook)) 

