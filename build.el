(require 'muse)
(require 'muse-latex)
(require 'muse-html)

(setq muse-xhtml-footer "
<!-- Page published by Emacs Muse ends here -->
  <div id=\"footer\" >&copy; Niels Giesen 2008.</div>
  </body>
</html>
"
      muse-xhtml-style-sheet
      "<link href=\"gimpmode.css\" rel=\"Stylesheet\" />")

;; generate pdf
(muse-publish-file
 (concat 
  (file-name-directory
   (or load-file-name buffer-file-name))
  "gimpmode.muse") 
 '("pdf"
   :final muse-latex-pdf-generate 
   :browser muse-latex-pdf-browse-file
   :link-suffix muse-latex-pdf-extension
   :osuffix muse-latex-pdf-extension 
   :base "latex")
 (file-name-directory
  (or load-file-name buffer-file-name)) t)

;; generate html
(muse-publish-file
 (concat 
  (file-name-directory
   (or load-file-name buffer-file-name))
  "gimpmode.muse")
 '("xhtml"
   :suffix muse-xhtml-extension
   :strings muse-xhtml-markup-strings
   :header muse-xhtml-header
   :footer muse-xhtml-footer
   :style-sheet muse-xhtml-style-sheet
   :base "html")
 (file-name-directory
  (or load-file-name buffer-file-name)) t)

(progn
  (find-file-literally "gimpmode.html")
  (goto-char (point-min))
  (while (re-search-forward "^\\([^>]\\)\\([^>]+\\)\\(</h2>\\)" nil t)  
    (replace-match "<span class=\"firstletter\">\\1</span>\\2\\3"))
  (save-buffer))