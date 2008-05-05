;; This is gimp-init.el

;; Put this in your .emacs (load-file "~/.emacs.d/gimp/gimp-init.el")
(let* ((this-dir (file-name-directory
		  (if load-file-name load-file-name buffer-file-name)))
       (related-dir (file-name-as-directory
		     (expand-file-name "related"
				       this-dir))))
  (add-to-list 'load-path this-dir)
  (add-to-list 'load-path related-dir))

(autoload 'run-gimp "gimp-mode" 
  "Inferior Gimp Interaction Mode Pimped for Emacs Lispniks" t)

(autoload 'gimp-mode "gimp-mode" 
  "Gimp Interaction Mode Pimped for Emacs Lispniks" t)

(autoload 'gimp-help "gimp-mode" 
  "Help for the Gimp" t)    

(autoload 'gimp-selector "gimp-mode" 
  "Help for the Gimp" t)    

;; Uncomment the following line to have a nice selector:
(global-set-key "\C-cg" 'gimp-selector)

(mapc (lambda (hook)
	(add-hook hook
	  (lambda ()
	    (require 'snippet)
	    (require 'scheme-complete)
	    (autoload 'eldoc-current-symbol "eldoc"))))
      '(gimp-mode-hook gimp-help-mode-hook inferior-gimp-mode-hook)) 

