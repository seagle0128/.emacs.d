;; init-prog.el --- Initialize prog configurations.	-*- lexical-binding: t -*-
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
    (add-hook 'after-init-hook #'global-prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("<=" . ?≤) prettify-symbols-alist)))))

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :init (add-hook 'after-init-hook #'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))

  (with-eval-after-load 'hydra
    (defhydra dumb-jump-hydra (:color blue :columns 3)
      "Dumb Jump"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back"))
    (bind-key "C-M-j" #'dumb-jump-hydra/body dumb-jump-mode-map)))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package quickrun
  :bind (("<f7>" . quickrun)
         ("C-c x" . quickrun)))

(use-package powershell)
(use-package csharp-mode)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

;; New `conf-toml-mode' in Emacs26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'after-init-hook #'editorconfig-mode))

;; Batch Mode eXtras (require 25+)
(if (>= emacs-major-version 25)
    (use-package bmx-mode
      :after company
      :diminish bmx-mode
      :init (bmx-mode-setup-defaults)))

(use-package fish-mode
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

(use-package swift-mode
  :config
  (with-eval-after-load 'flycheck
    (use-package flycheck-swift
      :init (flycheck-swift-setup))))

(use-package rust-mode
  :config (setq rust-format-on-save t))

(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
