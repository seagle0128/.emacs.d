;; init-web.el --- Initialize web configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
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

;;
;; css-mode
(use-package css-mode
  :defer t
  :config (setq css-indent-offset 2))
;;
;; scss mode
(use-package scss-mode
  :defer t
  :config
  (setq scss-compile-at-save nil)         ; Disable complilation on save
  )
;;
;; js mode
(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :interpreter "node"
  :config
  (add-hook 'js2-mode-hook
            '(lambda ()
               (setq js-indent-level 2)
               (js2-imenu-extras-mode 1)
               (ac-js2-mode 1)))
  )

;;
;; coffee mode
(use-package coffee-mode
  :defer t
  :config (setq coffee-tab-width 2))

;;
;; Web mode
(use-package web-mode
  :defer t
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\)$"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)

    (eval-after-load 'auto-complete
      '(add-to-list 'ac-modes 'web-mode))
    ))

;; Web beautify
(use-package web-beautify
  :defer t
  :bind
  ((:map js2-mode-map ("C-c C-b" . web-beautify-js))
   (:map json-mode-map "C-c C-b" . web-beautify-js)
   (:map sgml-mode-map "C-c C-b" . web-beautify-html)
   (:map css-mode-map "C-c C-b" . web-beautify-css)))

(use-package less-css-mode :defer t)
(use-package haml-mode :defer t)
(use-package php-mode :defer t)

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
