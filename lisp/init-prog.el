;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

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
;; General programming configurations.
;;

;;; Code:

(require 'init-custom)
(require 'init-const)

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist centaur-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))

  (with-no-warnings
    (if emacs/>=28p
        (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
              xref-show-definitions-function #'xref-show-definitions-completing-read)
      ;; Select from xref candidates with Ivy
      (use-package ivy-xref
        :after ivy
        :init
        (when emacs/>=27p
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
        (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))))

;; Jump to definition
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "anchor")
    :color blue :quit-key "q")
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Code formatting
;; Install: npm -g install prettier
(use-package prettier
  :diminish
  :hook (after-init . global-prettier-mode)
  :init (setq prettier-mode-sync-config-flag nil))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X" . quickrun)))

;; Browse devdocs.io documents using EWW
(when emacs/>=27p
  (use-package devdocs
    :bind (:map prog-mode-map
           ("M-<f1>" . devdocs-dwim))
    :init
    (defvar devdocs-major-mode-docs-alist
      '((c-mode . ("C"))
        (c++-mode . ("C++"))
        (python-mode . ("Python 3.9" "Python 3.8"))
        (ruby-mode . ("Ruby 3"))
        (go-mode . ("Go"))
        (rustic-mode . ("Rust"))
        (css-mode . ("CSS"))
        (html-mode . ("HTML"))
        (js-mode . ("JavaScript" "JQuery"))
        (js2-mode . ("JavaScript" "JQuery"))
        (emacs-lisp-mode . ("Elisp")))
      "Alist of MAJOR-MODE and list of docset names.")

    (mapc
     (lambda (e)
       (add-hook (intern (format "%s-hook" (car e)))
                 (lambda ()
                   (setq-local devdocs-current-docs (cdr e)))))
     devdocs-major-mode-docs-alist)

    (defun devdocs-dwim()
      "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
      (interactive)
      ;; Install the doc if it's not installed
      (mapc
       (lambda (str)
         (let* ((docs (split-string str " "))
                (doc (if (length= docs 1)
                         (downcase (car docs))
                       (concat (downcase (car docs)) "~" (downcase (cdr docs))))))
           (unless (and (file-directory-p devdocs-data-dir)
                        (directory-files devdocs-data-dir nil "^[^.]"))
             (message "Installing %s..." str)
             (devdocs-install doc))))
       (alist-get major-mode devdocs-major-mode-docs-alist))

      ;; Lookup the symbol at point
      (devdocs-lookup nil (thing-at-point 'symbol t)))))

(use-package cask-mode)
(use-package csharp-mode)
(use-package csv-mode)
(use-package julia-mode)
(use-package lua-mode)
(use-package mermaid-mode)
(use-package plantuml-mode)
(use-package powershell)
(use-package rmsbolt)                   ; A compiler output viewer
(use-package scala-mode)
(use-package swift-mode)
(use-package vimrc-mode)

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

;; Batch Mode eXtras
(use-package bmx-mode
  :after company
  :diminish
  :hook (after-init . bmx-mode-setup-defaults))

;; Fish shell
(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
