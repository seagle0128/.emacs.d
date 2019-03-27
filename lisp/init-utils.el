;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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

;; Youdao Dictionary
(use-package youdao-dictionary
  :functions (posframe-show posframe-hide)
  :preface
  (with-eval-after-load 'posframe
    (defun youdao-dictionary-search-at-point-posframe ()
      "Search word at point and display result with `posframe'."
      (interactive)
      (let ((word (youdao-dictionary--region-or-word)))
        (if word
            (progn
              (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (youdao-dictionary-mode)
                  (insert (youdao-dictionary--format-result word))
                  (goto-char (point-min))
                  (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
              (posframe-show youdao-dictionary-buffer-name
                             :position (point))
              (unwind-protect
                  (push (read-event) unread-command-events)
                (posframe-hide youdao-dictionary-buffer-name)))
          (message "Nothing to look up")))))

  (defun my-youdao-search-at-point ()
    (interactive)
    (if (display-graphic-p)
        (if (fboundp 'youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-posframe)
          (youdao-dictionary-search-at-point-tooltip))))
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . my-youdao-search-at-point))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;;
;; Search tools
;;

;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

;; `ripgrep'
(when (executable-find "rg")
  (use-package rg
    :defines projectile-command-map
    :hook (after-init . rg-enable-default-bindings)
    :config
    (setq rg-group-result t)
    (setq rg-show-columns t)

    (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

    (with-eval-after-load 'projectile
      (defalias 'projectile-ripgrep 'rg-project)
      (bind-key "s R" #'rg-project projectile-command-map))

    (with-eval-after-load 'counsel
      (bind-keys :map rg-global-map
                 ("c r" . counsel-rg)
                 ("c s" . counsel-ag)
                 ("c p" . counsel-pt)
                 ("c f" . counsel-fzf)))))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :init (setq atomic-chrome-buffer-open-style 'frame)
  :config
  (if (fboundp 'gfm-mode)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode)))))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Tramp
(use-package docker-tramp)

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; A Simmple and cool pomodoro timer
(use-package pomidor
  :bind ("<f12>" . pomidor)
  :init (setq alert-default-style (if sys/macp 'osx-notifier 'libnotify))
  :config
  (when sys/macp
    (setq pomidor-play-sound-file
          (lambda (file)
            (start-process "my-pomidor-play-sound"
                           nil
                           "afplay"
                           file)))))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ("C-x C-s" . my-save-buffer)))

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :preface
    (defun my-pdf-set-midnight-colors ()
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'default))))

    ;; Workaround for pdf-tools not reopening to last-viewed page
    ;; https://github.com/politza/pdf-tools/issues/18
    (defun my-pdf-set-last-viewed-bookmark ()
      (interactive)
      (when (eq major-mode 'pdf-view-mode)
        (bookmark-set (my-pdf-generate-bookmark-name))))

    (defun my-pdf-jump-last-viewed-bookmark ()
      (when (my-pdf-has-last-viewed-bookmark)
        (bookmark-jump (my-pdf-generate-bookmark-name))))

    (defun my-pdf-has-last-viewed-bookmark ()
      (assoc
       (my-pdf-generate-bookmark-name) bookmark-alist))

    (defun my-pdf-generate-bookmark-name ()
      (concat "LAST-VIEWED: " (buffer-name)))

    (defun my-pdf-set-all-last-viewed-bookmarks ()
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (my-pdf-set-last-viewed-bookmark))))
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward))
    :hook ((after-load-theme . my-pdf-set-midnight-colors)
           (kill-buffer . my-pdf-set-last-viewed-bookmark)
           (pdf-view-mode . my-pdf-jump-last-viewed-bookmark)
           (kill-emacs . (lambda ()
                           (unless noninteractive  ; as `save-place-mode' does
                             (my-pdf-set-all-last-viewed-bookmarks)))))
    :init (my-pdf-set-midnight-colors)
    :config (pdf-tools-install t nil t t)))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package htmlize)                   ; covert to html
(use-package list-environment)
(use-package memory-usage)
(use-package tldr)
(use-package ztree)                     ; text mode directory tree. Similar with beyond compare

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
