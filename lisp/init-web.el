;; init-web.el --- Initialize web configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.0.0
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

;; Css mode
(use-package css-mode
  :defer t
  :config (setq css-indent-offset 2))

;; Scss mode
(use-package scss-mode
  :defer t
  :config (setq scss-compile-at-save nil)         ; Disable complilation on save
  )

(use-package less-css-mode :defer t)

;; Css eldoc
(use-package css-eldoc
  :defer t
  :commands turn-on-css-eldoc
  :init (dolist (hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
          (add-hook hook 'turn-on-css-eldoc)))

;; Json mode
(use-package json-mode :defer t)

;; Improved JavaScript editing mode
(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :interpreter "node"
  :init (add-hook 'js2-mode-hook
                  '(lambda ()
                     (setq js2-basic-offset 2)
                     (js2-highlight-unused-variables-mode 1)
                     (js2-imenu-extras-mode 1)))
  :config
  (progn
    (use-package js2-refactor
      :defer t
      :diminish js2-refactor-mode
      :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
      :config (js2r-add-keybindings-with-prefix "C-c C-m"))

    (eval-after-load 'auto-complete
      '(use-package ac-js2
         :defer t
         :init (add-hook 'js2-mode-hook 'ac-js2-mode 1)))
    ))

;; Major mode for CoffeeScript code
(use-package coffee-mode
  :defer t
  :config (setq coffee-tab-width 2))

;; Typescript Interactive Development Environment
(use-package tide
  :defer t
  :diminish tide-mode
  :defines tide-format-options
  :init
  (progn
    (eval-after-load 'typescript-mode
      '(add-hook 'typescript-mode-hook #'tide-setup))

    (eval-after-load 'eldoc
      '(add-hook 'typescript-mode-hook #'eldoc-mode))

    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'tide-setup))

    (add-hook 'before-save-hook #'tide-format-before-save)
    (setq tide-format-options
          '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
            t
            :placeOpenBraceOnNewLineForFunctions
            nil))
    ))

;; Major mode for editing web templates
(use-package web-mode
  :defer t
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\)$"
  :defines ac-modes
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)

    (eval-after-load 'auto-complete
      '(add-to-list 'ac-modes 'web-mode))
    ))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :defer t
  :diminish skewer-mode skewer-css-mode skewer-html-mode
  :init
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook 'skewer-mode))
    (eval-after-load 'css-mode
      '(add-hook 'css-mode-hook 'skewer-css-mode))
    (eval-after-load 'sgml-mode
      '(add-hook 'html-mode-hook 'skewer-html-mode))
    ))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :defer t
  :init
  (progn
    (eval-after-load 'js2-mode
      '(bind-key "C-c C-b" 'web-beatify-js js2-mode-map))
    (eval-after-load 'json-mode
      '(bind-key "C-c C-b" 'web-beatify-js json-mode-map))
    (eval-after-load 'sgml-mode
      '(bind-key "C-c C-b" 'web-beatify-html html-mode-map))
    (eval-after-load 'css-mode
      '(bind-key "C-c C-b" 'web-beatify-css css-mode-map))
    )
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-"))
  )

(use-package haml-mode :defer t)
(use-package php-mode :defer t)

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
