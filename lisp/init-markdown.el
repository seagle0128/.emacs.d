;; init-markdown.el --- Initialize markdown configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.2.0
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
  (setq markdown-css-paths '("http://thomasf.github.io/solarized-css/solarized-light.min.css"))

  (use-package markdown-preview-mode
    :bind (:map markdown-mode-command-map
                ("P" . markdown-preview-mode))
    :config (setq markdown-preview-stylesheets markdown-css-paths))

  ;; Render and preview via `grip'
  (when (executable-find "grip")
    (defun markdown-to-html ()
      (interactive)
      (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
      (browse-url (format "http://localhost:5000/%s.%s"
                          (file-name-base)
                          (file-name-extension
                           (buffer-file-name)))))
    (bind-key "V" 'markdown-to-html markdown-mode-command-map)))

(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
