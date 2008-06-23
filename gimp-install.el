(let* ((gmd (file-name-directory
		       (or load-file-name buffer-file-name)))
       (gimp-dir
	(if (y-or-n-p 
	     (format "Does GIMP's config directory reside at %S? "
		     (expand-file-name "~/.gimp-2.4/")))
	    (expand-file-name "~/.gimp-2.4/")
	  (expand-file-name 
	   (read-file-name "Please enter config directory for the GIMP: "
			   "~/.gimp-2.4/"
			   "~/.gimp-2.4/"))))
       (gimp-emacs-dir (expand-file-name (concat gimp-dir "/emacs/")))
       (emacs-interaction.scm "emacs-interaction.scm")
       (emacs-interaction.scm-target
	(expand-file-name 
	 (mapconcat 'identity
		   (list gimp-dir
			 "scripts"
			 emacs-interaction.scm)
		   "/"))))
  (unless (file-exists-p gimp-emacs-dir)
    (message "Making directory %s for communication emacs<->gimp..."
	     gimp-emacs-dir
	     (make-directory gimp-emacs-dir)))
  (message "Installing %s..." emacs-interaction.scm-target)
  (apply
   (if (fboundp 'make-symbolic-link)
       'make-symbolic-link
     'copy-file)
   (list 
    (expand-file-name (concat gmd emacs-interaction.scm))
    emacs-interaction.scm-target
    t))
  (load (concat gmd "gimp-init.el"))
  (byte-compile-file 
   (concat gmd "gimp-mode.el"))
  (message "
Installation of gimp-mode completed.
====================================

Put the following in your load file: %S

Please see the file README for further instructions."
	   `(load ,(concat gmd "gimp-init.el"))))



