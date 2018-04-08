;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-
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
;;             Web configurations.
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

;; CSS mode
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :init
  (dolist (hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
    (add-hook hook #'turn-on-css-eldoc)))

;; JSON mode
(use-package json-mode)

;; Improved JavaScript editing mode
(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)
  :config
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for CoffeeScript code
(use-package coffee-mode
  :config (setq coffee-tab-width 2))

;; Typescript Interactive Development Environment
(use-package tide
  :diminish tide-mode
  :init
  (defun setup-tide-mode ()
    "Setup tide mode."
    (interactive)
    (tide-setup)
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1))

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook #'setup-tide-mode))

  (add-hook 'before-save-hook #'tide-format-before-save)
  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
          t
          :placeOpenBraceOnNewLineForFunctions
          nil))

  (with-eval-after-load 'company
    (cl-pushnew (company-backend-with-yas 'company-tide) company-backends)))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Complete for web,html,emmet,jade,slim modes
  (with-eval-after-load 'company
    (use-package company-web
      :init
      (cl-pushnew (company-backend-with-yas 'company-web-html) company-backends)
      (cl-pushnew (company-backend-with-yas 'company-web-jade) company-backends)
      (cl-pushnew (company-backend-with-yas 'company-web-slim) company-backends))))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish skewer-mode
  :init
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook #'skewer-mode))
  (with-eval-after-load 'css-mode
    (add-hook 'css-mode-hook #'skewer-css-mode))
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'skewer-html-mode))
  (with-eval-after-load 'sgml-mode
    (add-hook 'html-mode-hook #'skewer-html-mode))

  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :init
  (with-eval-after-load 'js2-mode
    (bind-key "C-c C-b" 'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c C-b" 'web-beautify-js json-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c C-b" 'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c C-b" 'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

(use-package haml-mode)
(use-package php-mode)

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
