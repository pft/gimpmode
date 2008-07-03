;; (require 'muse)
;; (require 'muse-latex)
;; (require 'muse-html)

;; (setq muse-project-alist
;;       '(("Website" ("gimpmode.muse" :default "index")
;; 	 (:base "html" :path "~/public_html")
;; 	 (:base "pdf" :path "~/public_html/pdf"))))

;; (muse-project-publish "Website")

(progn
  (find-file-literally "gimpmode.html")
  (goto-char (point-min))
  (while (re-search-forward "^\\([^>]\\)\\([^>]+\\)\\(</h2>\\)" nil t)  
    (replace-match "<span class=\"firstletter\">\\1</span>\\2\\3"))
  (save-buffer))