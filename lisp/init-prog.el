;; init-prog.el --- Initialize prog configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.2.0
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

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package quickrun)
(use-package powershell)
(use-package csharp-mode)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'prog-mode-hook 'editorconfig-mode))

;; Supports batch-mode only on >=25
(when (< emacs-major-version 25)
  (use-package batch-mode
    :mode (("\\.\\(cmd\\|bat\\)$" . batch-mode))))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; On the fly markdown preview
  (use-package flymd
    :bind (:map markdown-mode-map ("C-c C-c f" . flymd-flyit))))

(use-package fish-mode
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        'fish_indent-before-save))))

(use-package swift-mode
  :config
  (with-eval-after-load 'flycheck
    (use-package flycheck-swift
      :init (flycheck-swift-setup))))

(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
