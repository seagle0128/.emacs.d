;; init-markdown.el --- Initialize markdown configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.3.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Markdown configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  (with-eval-after-load 'flycheck
    (defun set-markdownlint-config ()
      "Set the `mardkownlint' config file for the current buffer."
      (when (and (executable-find "markdownlint") buffer-file-name)
        (let ((md-lint ".markdownlint.json"))
          (let ((md-lint-dir (locate-dominating-file buffer-file-name md-lint)))
            (if md-lint-dir
                (setq-local flycheck-markdown-markdownlint-cli-config (concat md-lint-dir md-lint)))))))
    (add-hook 'markdown-mode-hook #'set-markdownlint-config))

  ;; Preview
  (setq markdown-css-paths '("http://thomasf.github.io/solarized-css/solarized-light.min.css"))

  (use-package markdown-preview-mode
    :bind (:map markdown-mode-command-map
                ("P" . markdown-preview-mode))
    :config
    (setq markdown-preview-stylesheets
          (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
                "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
    (setq markdown-preview-javascript
          (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
")))

  ;; Render and preview via `grip'
  (when (executable-find "grip")
    (defun markdown-to-html ()
      (interactive)
      (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
      (browse-url (format "http://localhost:5000/%s.%s"
                          (file-name-base)
                          (file-name-extension
                           (buffer-file-name)))))
    (bind-key "V" #'markdown-to-html markdown-mode-command-map)))

(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
