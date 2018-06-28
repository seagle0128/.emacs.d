;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
;; Some usefule Utilities.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

;; A tree layout file explorer
(use-package treemacs
  :commands (treemacs-follow-mode treemacs-filewatch-mode treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-c 1"     . treemacs-delete-other-windows))
  :config
  (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;; Projectile integration for treemacs
(use-package treemacs-projectile
  :after projectile
  :bind (([M-f8] . treemacs-projectile)
         :map projectile-command-map
         ("h" . treemacs-projectile)))

;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; Search utils: `ag', `rg', `pt'
(use-package ag
  :init
  (with-eval-after-load 'projectile
    (bind-key "s s" #'ag-project projectile-command-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package pt
  :init
  (with-eval-after-load 'projectile
    (bind-key "s p" #'projectile-pt projectile-command-map))
  :config (use-package wgrep-pt))

(use-package rg
  :defines counsel-projectile-command-map
  :requires wgrep-ag-setup
  :hook ((after-init . rg-enable-default-bindings)
         (rg-mode . wgrep-ag-setup))
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (bind-key "s r" #'rg-project projectile-command-map))

  (when (fboundp 'ag)
    (bind-key "a" #'ag rg-global-map))
  (when (fboundp 'pt-regexp)
    (bind-key "P" #'pt-regexp rg-global-map))

  (with-eval-after-load 'counsel
    (bind-key "c r" #'counsel-rg rg-global-map)
    (bind-key "c s" #'counsel-ag rg-global-map)
    (bind-key "c p" #'counsel-pt rg-global-map)
    (bind-key "c f" #'counsel-fzf rg-global-map))

  (with-eval-after-load 'counsel-projectile
    (bind-key "s r" #'rg-project counsel-projectile-command-map)))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :after markdown-mode
  :hook (after-init . atomic-chrome-start-server)
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode))))

;; Tramp
(use-package docker-tramp)

;; Emoji
(when centaur-emoji-enabled
  (use-package emojify
    :hook (after-init . global-emojify-mode)
    :config
    (with-eval-after-load 'company
      (use-package company-emoji
        :defines company-backends
        :init (add-to-list 'company-backends 'company-emoji)))))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; A Simmple and cool pomodoro timer
(use-package pomidor
  :bind (("<f12>" . pomidor)))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package htmlize)                   ; covert to html
(use-package list-environment)
(use-package memory-usage)
(use-package open-junk-file)
(use-package try)
(use-package ztree)                     ; text mode directory tree. Similar with beyond compare

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
