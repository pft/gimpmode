;;; gimp-init.el --- Loading file for Gimp Mode

;; Copyright (C) 2008-2012 Niels Giesen

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: processes, languages, multimedia, tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

 ;; Check whether first time
;; Put this in your .emacs (load-file "~/.emacs.d/gimp/gimp-init.el")
(defvar gimp-mode-dir 
  (file-name-directory
   (or load-file-name buffer-file-name)))

(condition-case err
    (load (concat gimp-mode-dir "gimp-vars.el"))
  (error (load-file (concat gimp-mode-dir "gimp-install.el"))))

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

(autoload 'gimp-install "gimp-install.el")
;; Uncomment the following line to have a nice selector:
;(global-set-key "\C-cg" 'gimp-selector)

(mapc (lambda (hook)
	(add-hook hook
	  (lambda ()
	    (require 'snippet)
	    (require 'scheme-complete)
	    (autoload 'eldoc-current-symbol "eldoc"))))
      '(gimp-mode-hook gimp-help-mode-hook inferior-gimp-mode-hook)) 

(add-to-list 'auto-mode-alist
	     '("\\(s-f-\\|script-fu\\).*\\.scm\\'" . gimp-mode))

(defun gimp-script-p ()
  "Return t when current buffer holds a GIMP script."
  (when (and (buffer-file-name) (string-match "gimp.*\.scm\\'" (buffer-file-name)))
    t))

(add-to-list 'magic-mode-alist 
	     '(gimp-script-p . gimp-mode))

;; auto-insert-alist is *not* automatically loaded in a vanilla
;; session... force by toggling:
(auto-insert-mode)
(auto-insert-mode)

(add-to-list 
 'auto-insert-alist
 '(gimp-mode 
   lambda nil
   (insert ";; -*- mode: Gimp; -*-\n")
   (snippet-insert (eval gimp-registration-snippet))))

