;; init-prog.el --- Initialize prog configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.1.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Configurations for prog mode.
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

(use-package quickrun :defer t)
(use-package markdown-mode :defer t)
(use-package powershell :defer t)
(use-package csharp-mode :defer t)
(use-package dockerfile-mode :defer t :mode "Dockerfile\\'")

(use-package editorconfig
  :defer t
  :diminish editorconfig-mode
  :init (add-hook 'prog-mode-hook 'editorconfig-mode))

(use-package dos
  :defer t
  :init (add-to-list 'auto-mode-alist
                     '("\\.\\(cmd\\|bat\\|btm\\)$" . dos-mode)))

(use-package fish-mode
  :defer t
  :init
  (progn
    (add-hook 'fish-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'fish_indent-before-save)))
    (eval-after-load 'auto-complete
      '(add-hook 'fish-mode-hook 'auto-complete-mode))
    ))

(use-package robot-mode
  :ensure nil
  :defer t
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
