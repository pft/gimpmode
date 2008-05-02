;; This is gimp-init.el

;; Put this in your .emacs (load-file "~/.emacs.d/elisp/gimp/gimp-init.el")

;; Put gimp.el (and scheme-complete.el) somewhere in your `load-path'
;; or add the parent dir of gimp.el to your `load-path'.
(let* ((this-dir (file-name-directory
		  (if load-file-name load-file-name buffer-file-name)))
       (related-dir (file-name-as-directory
		     (expand-file-name "related"
				       this-dir))))
  (add-to-list 'load-path this-dir)
  (add-to-list 'load-path related-dir))

(autoload 'run-gimp "gimp" 
  "Inferior Gimp Interaction Mode Pimped for Emacs Lispniks" t)

(autoload 'gimp-mode "gimp" 
  "Gimp Interaction Mode Pimped for Emacs Lispniks" t)

(autoload 'gimp-help "gimp" 
  "Help for the Gimp" t)    

(global-set-key "\C-cg" 'gimp-selector)
;; Uncomment the below if you have scheme-complete.el in your
;; load-path and want to get echoing for core scheme functions too:

(mapc (lambda (hook)
	(add-hook hook
	  (lambda ()
	    (require 'snippet)
	    (require 'scheme-complete)
	    (autoload 'eldoc-current-symbol "eldoc"))))
      '(gimp-mode-hook gimp-help-mode-hook inferior-gimp-mode-hook)) 

