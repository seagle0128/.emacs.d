;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; ---------------------------------------------------------------------------
;; Code Display & Utilities
;; ---------------------------------------------------------------------------

;; Prettify Symbols (e.g., display “lambda” as “λ”)
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist centaur-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter support
(when (centaur-treesit-available-p)
  ;; Automatic Tree-sitter grammar management
  (use-package treesit-auto
    :functions centaur-treesit-available-p
    :hook (after-init . global-treesit-auto-mode)
    :init (setq treesit-auto-install 'prompt)))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish
  :functions childframe-workable-p
  :commands eldoc-box-hover-mode
  :config
  (use-package eldoc-box
    :custom
    (eldoc-box-lighter nil)
    (eldoc-box-only-multi-line t)
    (eldoc-box-clear-with-C-g t)
    :custom-face
    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
    (eldoc-box-body ((t (:inherit tooltip))))
    :hook (eglot-managed-mode . (lambda ()
                                  (if (childframe-workable-p)
                                      (eldoc-box-hover-mode 1)
                                    (eldoc-box-hover-mode -1))))))

;; Cross-referencing commands
(use-package xref
  :autoload xref-show-definitions-completing-read
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Code styles
(use-package editorconfig
  :diminish
  :hook after-init)

;; Reformat buffer stably
(use-package apheleia
  :diminish
  :hook (after-init . apheleia-global-mode))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; Browse devdocs.io documents using EWW
(use-package devdocs
  :autoload devdocs--available-docs
  :commands (devdocs-install devdocs-lookup)
  :bind (:map prog-mode-map
         ("M-<f1>" . devdocs-dwim)
         ("C-h D"  . devdocs-dwim))
  :init
  (defvar devdocs-major-mode-docs-alist
    '(((c-mode c-ts-mode)           . ("c"))
      ((c++-mode c++-ts-mode)       . ("cpp"))
      ((css-mode css-ts-mode)       . ("css"))
      (emacs-lisp-mode              . ("elisp"))
      ((html-mode html-ts-mode)     . ("html"))
      ((js-mode js-ts-mode)         . ("javascript"))
      ((python-mode python-ts-mode) . ("python~3.14"))
      ((ruby-mode ruby-ts-mode)     . ("ruby~3"))
      ((rust-mode rust-ts-mode)     . ("rust")))
    "Alist of major-mode and docs.")

  (mapc (lambda (item)
          (let ((modes (car item))
                (docs  (cdr item)))
            (when (nlistp modes)
              (setq modes (list modes)))
            (dolist (m modes)
              (add-hook (intern (format "%s-hook" m))
                        (lambda ()
                          (setq-local devdocs-current-docs docs))))))
        devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

;; ---------------------------------------------------------------------------
;; Miscellaneous Programming Modes
;; ---------------------------------------------------------------------------
(use-package cask-mode)
(use-package cmake-mode)
(use-package csv-mode)
(use-package cue-sheet-mode)
(use-package dart-mode)
(use-package lua-mode)
(use-package v-mode)
(use-package vimrc-mode)

(use-package powershell
  :custom (explicit-pwsh.exe-args explicit-powershell.exe-args))

(if (centaur-treesit-available-p)
    (progn
      (use-package julia-ts-mode)
      (use-package mermaid-ts-mode
        :mode ("\\.mmd\\'" . mermaid-ts-mode))
      (use-package scala-ts-mode)
      (use-package swift-ts-mode
        :mode ("\\.swift\\'" . swift-ts-mode))
      (use-package yaml-ts-mode
        :mode ("\\.ya?ml\\'" . yaml-ts-mode)))
  (progn
    (use-package julia-mode)
    (use-package mermaid-mode)
    (use-package scala-mode)
    (use-package swift-mode)
    (use-package yaml-mode)))

;; Protobuf item configuration
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           "Set up Protobuf's imenu generic expressions."
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;; nXML item for special file types
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml\\'" . xml-mode)))

;; Fish shell item and auto-formatting
(use-package fish-mode
  :commands fish_indent-before-save
  :defines eglot-server-programs
  :hook (fish-mode . (lambda ()
                       "Integrate `fish_indent` formatting with Fish shell mode."
                       (add-hook 'before-save-hook #'fish_indent-before-save)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(fish-mode . ("fish-lsp" "start")))))

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
