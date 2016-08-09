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
  :init
  (progn
    (add-hook 'css-mode-hook 'turn-on-css-eldoc)
    (add-hook 'scss-mode-hook 'turn-on-css-eldoc)
    (add-hook 'less-css-mode-hook 'turn-on-css-eldoc)
    ))

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
  :commands eldoc-mode
  :config
  (progn
    (add-hook 'typescript-mode-hook #'tide-setup)
    (add-hook 'typescript-mode-hook #'eldoc-mode)
    (add-hook 'before-save-hook 'tide-format-before-save)

    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'tide-setup))

    (setq tide-format-options
          '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
            t
            :placeOpenBraceOnNewLineForFunctions
            nil))
    ))

;; Major mode for editing web templates
(use-package web-mode
  :defer t
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\)$"
  :defines ac-modes aggressive-indent-excluded-modes
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)

    (eval-after-load 'auto-complete
      '(add-to-list 'ac-modes 'web-mode))

    (eval-after-load 'aggressvie-indent
      '(add-to-list 'aggressive-indent-excluded-modes 'web-mode))
    ))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :defer t
  :diminish skewer-mode
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
      '(define-key js2-mode-map (kbd "C-c C-b") 'web-beautify-js))
    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c C-b") 'web-beautify-js))
    (eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd "C-c C-b") 'web-beautify-html))
    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c C-b") 'web-beautify-css))
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
