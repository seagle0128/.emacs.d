;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

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

(require 'init-custom)
(require 'init-const)
(require 'init-funcs)

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist centaur-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter support
;; @see https://github.com/casouri/tree-sitter-module
;;      https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
(use-package treesit
  :ensure nil
  :when (and centaur-tree-sitter (centaur-treesit-available-p))
  :init (setq major-mode-remap-alist
              '((c-mode          . c-ts-mode)
                (c++-mode        . c++-ts-mode)
                (cmake-mode      . cmake-ts-mode)
                (conf-toml-mode  . toml-ts-mode)
                (csharp-mode     . csharp-ts-mode)
                (css-mode        . css-ts-mode)
                (dockerfile-mode . dockerfile-ts-mode)
                (go-mode         . go-ts-mode)
                (java-mode       . java-ts-mode)
                (json-mode       . json-ts-mode)
                (js-json-mode    . json-ts-mode)
                (js-mode         . js-ts-mode)
                (python-mode     . python-ts-mode)
                (rust-mode       . rust-ts-mode)
                (sh-mode         . bash-ts-mode)
                (typescript-mode . typescript-ts-mode))))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :config
  (cond
   ((executable-find "ugrep")
    (grep-apply-setting
     'grep-command "ugrep --color=auto -0In -e ")
    (grep-apply-setting
     'grep-template "ugrep --color=auto -0In -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
    (grep-apply-setting
     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
   ((executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :config
  (with-no-warnings
    ;; Use faster search tool
    (when emacs/>=28p
      (add-to-list 'xref-search-program-alist
                   '(ugrep . "xargs -0 ugrep <C> --null -ns -e <R>"))
      (cond
       ((executable-find "ugrep")
        (setq xref-search-program 'ugrep))
       ((executable-find "rg")
        (setq xref-search-program 'ripgrep))))

    ;; Select from xref candidates with Ivy
    (if emacs/>=28p
        (setq xref-show-definitions-function #'xref-show-definitions-completing-read
              xref-show-xrefs-function #'xref-show-definitions-completing-read)
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
    :color blue :quit-key ("q" "C-g"))
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

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; Browse devdocs.io documents using EWW
(when emacs/>=27p
  (use-package devdocs
    :autoload (devdocs--installed-docs devdocs--available-docs)
    :bind (:map prog-mode-map
           ("M-<f1>" . devdocs-dwim)
           ("C-h D"  . devdocs-dwim))
    :init
    (defconst devdocs-major-mode-docs-alist
      '((c-mode          . ("c"))
        (c++-mode        . ("cpp"))
        (python-mode     . ("python~3.10" "python~2.7"))
        (ruby-mode       . ("ruby~3.1"))
        (go-mode         . ("go"))
        (rustic-mode     . ("rust"))
        (css-mode        . ("css"))
        (html-mode       . ("html"))
        (julia-mode      . ("julia~1.8"))
        (js-mode         . ("javascript" "jquery"))
        (js2-mode        . ("javascript" "jquery"))
        (emacs-lisp-mode . ("elisp")))
      "Alist of major-mode and docs.")

    (mapc
     (lambda (mode)
       (add-hook (intern (format "%s-hook" (car mode)))
                 (lambda ()
                   (setq-local devdocs-current-docs (cdr mode)))))
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
      (devdocs-lookup nil (thing-at-point 'symbol t)))))

;; Misc. programming modes
(when emacs/>=27p
  (use-package csv-mode))

(unless emacs/>=29p
  (use-package csharp-mode))

(use-package cask-mode)
(use-package cmake-mode)
(use-package groovy-mode)
(use-package julia-mode)
(use-package lua-mode)
(use-package mermaid-mode)
(use-package plantuml-mode)
(use-package rmsbolt)                   ; A compiler output viewer
(use-package scala-mode)
(use-package swift-mode)
(use-package v-mode)
(use-package vimrc-mode)

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

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
