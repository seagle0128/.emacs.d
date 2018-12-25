;;; init-mini.el --- Centaur Emacs minimal configurations.	-*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
;; Version: 5.0.0
;; Keywords: .emacs.d centaur

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
;; Centaur Emacs minimal configurations for debugging purpose.
;;

;;; Code:

;; Load path
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq-default kill-whole-line t)           ; Kill line including '\n'

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; UI
(tool-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(load-theme 'wombat t)

(global-hl-line-mode 1)
(if (fboundp 'display-line-numbers-mode)
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))

;; Basic modes
(ido-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Keybindings
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
