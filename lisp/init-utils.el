;; init-utils.el --- Initialize basic configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Utils configurations.
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

(require 'init-const)

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))
(bind-key "<f5>" 'revert-current-buffer)

;; Update configurations
(defun update-config ()
  "Update Emacs configurations to the latest version."
  (interactive)
  (message "Updating Emacs configurations...")
  (cd "~/.emacs.d/")
  (shell-command "git pull")
  (message "Update finished. Restart Emacs to complete the process."))

;; Save a file as utf-8
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;; Proxy settings
(defun toggle-proxy ()
  "Toggle network(htpp/https) proxy."
  (interactive)
  (if url-proxy-services
      (progn
        (setq url-proxy-services nil)
        (message "No proxy"))
    (progn
      (setq my-proxy "127.0.0.1:1087")
      (setq url-proxy-services `(("http" . ,my-proxy)
                                 ("https" . ,my-proxy)))
      (message "Set proxy to %s" my-proxy))))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook 'which-key-mode))

;; Context-sensitive external browse URL or Internet search
(use-package browse-url-dwim
  :init (add-hook 'after-init-hook 'browse-url-dwim-mode))

;; Tree explorer
(use-package neotree
  :bind (([f7] . neotree-toggle)
         :map neotree-mode-map
         ("i" . neotree-enter-horizontal-split)
         ("I" . neotree-enter-vertical-split))
  :init
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char)))

;; Show imenu entries in a seperate buffer
(use-package imenu-list
  :bind ([f8] . imenu-list-smart-toggle)
  :init (setq imenu-list-focus-after-activation t))

;; Dash
;; only avaliable on macOS
(when sys/macp
  (use-package dash-at-point
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  (with-eval-after-load 'popwin
    (push "*Youdao Dictionary*" popwin:special-display-config))

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; Search
(use-package ag
  :bind (("C-c s" . ag))
  :init
  (with-eval-after-load 'projectile
    (bind-key "C-c p s s" 'ag-project projectile-mode-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package rg
  :bind (("C-c r" . rg)
         ("C-c m" . rg-dwim))
  :init
  (if (fboundp 'wgrep-ag-setup)
      (add-hook 'rg-mode-hook 'wgrep-ag-setup))
  (with-eval-after-load 'projectile
    (bind-key "C-c p s r" 'rg-project projectile-mode-map))
  :config
  ;; FIXME: Invalid processing regexp while searching words including "("
  ;; https://github.com/dajva/rg.el/issues/19#issuecomment-328291803
  (setq rg-command-line-flags '("--fixed-strings"))

  (setq rg-custom-type-aliases nil)
  (setq rg-group-result t)
  (setq rg-show-columns t))

;; Jump to definition via ag/rg/grep
(use-package dumb-jump
  :init (add-hook 'after-init-hook 'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy (setq dumb-jump-selector 'ivy)))

;; Side-by-side diff view
(use-package diffview)

;; Text mode directory tree. Similar with beyond compare
(use-package ztree)

;; Extensions to `Info'
(use-package info+
  :after info
  :init (setq Info-fontify-angle-bracketed-flag nil))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; Log keyboard commands to buffer
(use-package command-log-mode
  :diminish (command-log-mode . "¢")
  :init (setq command-log-mode-auto-show t))

;; A Simmple and cool pomodoro timer
(use-package pomidor
  :bind (("<f12>" . pomidor)))

;; Emacs StartUp Profiler
(use-package esup)

;; Misc
(use-package copyit)
(use-package htmlize)
(use-package list-environment)
(use-package memory-usage)
(use-package open-junk-file)
(use-package try)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
