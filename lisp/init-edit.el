;; init-edit.el --- Initialize edit configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Edit configurations.
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

;; CUA mode
(use-package cua-base
  :defer t
  :config
  (setq cua-enable-cua-keys nil)           ;; don't add C-x,C-c,C-v
  (cua-mode t)                             ;; for rectangles, CUA is nice
  )

;; Automatically reload files was modified by external program
(use-package autorevert
  :defer t
  :diminish auto-revert-mode
  :config (global-auto-revert-mode 1))

;; Ace jump mode
(use-package ace-jump-mode
  :defer t
  :bind ("C-c SPC" . ace-jump-mode))

;; Ace link
(use-package ace-link
  :defer t
  :config (ace-link-setup-default))

;; Aggressive indent
(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

;; Auto indent
(use-package auto-indent-mode
  :defer t
  :diminish auto-indent-mode
  :config
  (setq auto-indent-assign-indent-level-variables nil)
  (setq python-indent-guess-indent-offset nil) ; fix python indent compatibility issue
  (setq auto-indent-indent-style 'conservative)
  (auto-indent-global-mode 1))

;; Anzu mode
(use-package anzu
  :defer t
  :diminish anzu-mode
  :config (global-anzu-mode 1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;; Mwim
(use-package mwim
  :defer t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; Pager
(use-package pager
  :defer t
  :config
  (require 'pager-default-keybindings))

;; Move text
(use-package move-text
  :defer t
  :config (move-text-default-bindings))

;; Comment
(use-package comment-dwim-2
  :defer t
  :bind ("M-;" . comment-dwim-2))

;; IEdit
(use-package iedit
  :defer t
  :bind
  (("C-;" . iedit-mode)
   ("C-x r RET" . iedit-rectangle-mode)))

;; Back button
(use-package back-button
  :defer t
  :diminish back-button-mode
  :config (back-button-mode 1))

;; Undo Tree
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

;; Multiple cursors
(use-package multiple-cursors
  :defer t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<". mc/mark-previous-like-this)
   ("C-c C-<". mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (global-set-key (kbd "M-<down-mouse-1>") 'setq))

;; Expand region
(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; Subword and Superword
(use-package subword
  :defer t
  :diminish subword-mode
  :init (add-hook 'prog-mode-hook 'subword-mode))

;; Smartparens
(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (add-hook 'web-mode-hook
            '(lambda ()
               "Workaround for auto-paring issues for Rails and Django."
               (sp-local-pair 'web-mode "{" "}" :actions nil)
               (sp-local-pair 'web-mode "<" ">" :actions nil)))
  )

;; Swoop
(use-package swoop
  :defer t
  :config
  (setq swoop-font-size-change: nil)
  :bind
  (("C-o" . swoop)
   ("C-M-o" . swoop-multi)
   ("M-o" . swoop-pcre-regexp)
   ("C-S-o" . swoop-back-to-last-position)))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
