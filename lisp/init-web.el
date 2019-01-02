;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
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

;;; Commentary:
;;
;; Web configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode)

;; Improved JavaScript editing mode
(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (if centaur-lsp
      (unbind-key "M-." js2-mode-map))

  (with-eval-after-load 'flycheck
    (if (or (executable-find "eslint_d")
            (executable-find "eslint")
            (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
    (if (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js2-refactor
    :diminish js2-refactor-mode
    :hook (js2-mode . js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for CoffeeScript code
(use-package coffee-mode
  :config (setq coffee-tab-width 2))

;; Typescript Interactive Development Environment
(unless centaur-lsp
  (use-package tide
    :diminish tide-mode
    :defines (company-backends tide-format-options)
    :functions (tide-setup tide-hl-identifier-mode)
    :preface
    (defun setup-tide-mode ()
      "Setup tide mode."
      (interactive)
      (tide-setup)
      (eldoc-mode 1)
      (tide-hl-identifier-mode 1))
    :hook (((typescript-mode js2-mode) . setup-tide-mode)
           (before-save . tide-format-before-save))
    :config
    (setq tide-format-options
          '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
            t
            :placeOpenBraceOnNewLineForFunctions
            nil))

    (with-eval-after-load 'company
      (cl-pushnew 'company-tide company-backends))))

;; Major mode for editing web templates
(use-package web-mode
  :defines company-backends
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Complete for web,html,emmet,jade,slim modes
  (unless centaur-lsp
    (use-package company-web
      :after company
      :init (dolist (mode '(company-web-html company-web-jade company-web-slim))
              (cl-pushnew mode company-backends)))))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
;; Insta;; npm -g install js-beautify
(use-package web-beautify
  :init
  (with-eval-after-load 'js-mode
    (bind-key "C-c b" #'web-beautify-js js-mode-map))
  (with-eval-after-load 'js2-mode
    (bind-key "C-c b" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c b" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c b" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c b" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c b" #'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

(use-package haml-mode)
(use-package php-mode)

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
