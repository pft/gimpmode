;; This is gimp-init.el

;; Put this in your .emacs (load-file "~/.emacs.d/gimp/gimp-init.el")
(defvar gimp-mode-dir 
  (file-name-directory
		  (or load-file-name buffer-file-name)))

(let* ((related-dir (file-name-as-directory
		     (expand-file-name "related"
				       gimp-mode-dir))))
  (add-to-list 'load-path gimp-mode-dir)
  (add-to-list 'load-path related-dir))

(mapc (lambda (command)
	(autoload command "gimp-mode" "" t))
      '(run-gimp
	gimp-mode
	gimp-help
	gimp-selector
	gimp-open-image
	gimp-cl-connect))    
;; Uncomment the following line to have a nice selector:
;(global-set-key "\C-cg" 'gimp-selector)

(mapc (lambda (hook)
	(add-hook hook
	  (lambda ()
	    (require 'snippet)
	    (require 'scheme-complete)
	    (autoload 'eldoc-current-symbol "eldoc"))))
      '(gimp-mode-hook gimp-help-mode-hook inferior-gimp-mode-hook)) 

