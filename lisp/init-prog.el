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

(use-package prog-mode
  :ensure nil
  :init
  ;; Prettify Symbols
  ;; e.g. display “lambda” as “λ”
  (when (boundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook 'global-prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("<=" . ?≤) prettify-symbols-alist)))))

(use-package quickrun)
(use-package markdown-mode)
(use-package powershell)
(use-package csharp-mode)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'prog-mode-hook 'editorconfig-mode))

(use-package dos
  :init (add-to-list 'auto-mode-alist
                     '("\\.\\(cmd\\|bat\\|btm\\)$" . dos-mode)))

(use-package fish-mode
  :init
  (add-hook 'fish-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook 'fish_indent-before-save)))
  (eval-after-load 'auto-complete
    '(add-hook 'fish-mode-hook 'auto-complete-mode)))

(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

;; Eval interface for various REPLs
(use-package eval-in-repl
  :bind (:map emacs-lisp-mode-map
              ("<C-M-return>" . eir-eval-in-ielm)
              :map lisp-interaction-mode-map
              ("<C-M-return>" . eir-eval-in-ielm)
              :map Info-mode-map
              ("<C-M-return>" . eir-eval-in-ielm)
              :map lisp-mode-map
              ("<C-M-return>" . eir-eval-in-slime))
  :init
  (setq eir-ielm-eval-in-current-buffer t)

  (eval-after-load 'sh-script
    '(bind-key "<C-M-return>" 'eir-eval-in-shell sh-mode-map))
  (eval-after-load 'python-mode
    '(bind-key "<C-M-return>" 'eir-eval-in-python python-mode-map))
  (eval-after-load 'ruby-mode
    '(bind-key "<C-M-return>" 'eir-eval-in-ruby ruby-mode-map))
  (eval-after-load 'js2-mode
    '(bind-key "<C-M-return>" 'eir-eval-in-javascript js2-mode-map)))

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
