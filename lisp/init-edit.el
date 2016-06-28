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

(eval-when-compile (require 'cua-base))
(eval-when-compile (require 'multiple-cursors))

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
;; (setq-default kill-whole-line t)           ; Kill line including '\n'

(delete-selection-mode 1)
(electric-pair-mode 1)

(add-hook 'abbrev-mode-hook '(lambda () (diminish 'abbrev-mode)))

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (diminish 'auto-fill-function)))

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Display “lambda” as “λ”
(when (boundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode 1))

;; Encoding
(set-language-environment 'Chinese-GB18030)
(set-clipboard-coding-system 'chinese-gb18030)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(set-file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; CUA mode
(setq cua-enable-cua-keys nil)           ;; don't add C-x,C-c,C-v
(cua-mode t)                             ;; for rectangles, CUA is nice

;; Automatically reload files was modified by external program
(use-package autorevert
  :defer t
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook 'global-auto-revert-mode))

;; Avy
(use-package avy
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config (avy-setup-default))

;; zzz to char
(use-package zzz-to-char
  :defer t
  :bind (("M-z" . zzz-to-char)))

;; Ace link
(use-package ace-link
  :defer t
  :init (add-hook 'after-init-hook 'ace-link-setup-default))

;; Aggressive indent
(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init (add-hook 'after-init-hook 'global-aggressive-indent-mode))

;; Auto indent
(use-package auto-indent-mode
  :defer t
  :diminish auto-indent-mode
  :init
  (progn
    (setq auto-indent-assign-indent-level-variables nil)
    (setq auto-indent-indent-style 'conservative)
    (add-hook 'after-init-hook 'auto-indent-global-mode)
    ))

;; Anzu mode
(use-package anzu
  :defer t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode 1))

;; Mwim
(use-package mwim
  :defer t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Pager
(use-package pager
  :defer t
  :commands pager-page-down pager-page-up pager-row-down pager-row-up
  :bind (("\C-v"    . pager-page-down)
         ([next]    . pager-page-down)
         ("\ev"     . pager-page-up)
         ([prior]   . pager-page-up)
         ([M-up]   . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

;; Move text
(use-package move-text
  :defer t
  :init (add-hook 'after-init-hook 'move-text-default-bindings))

;; Comment
(use-package comment-dwim-2
  :defer t
  :bind ("M-;" . comment-dwim-2))

;; IEdit
(use-package iedit
  :defer t
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)))

;; Back button
(use-package back-button
  :defer t
  :diminish back-button-mode
  :init (add-hook 'after-init-hook 'back-button-mode))

;; Undo Tree
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook 'global-undo-tree-mode))

;; Multiple cursors
(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<". mc/mark-previous-like-this)
         ("C-c C-<". mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

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
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)

              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp))
  :init
  (progn
    (add-hook 'after-init-hook 'smartparens-global-mode)
    (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode))
  :config
  (progn
    (require 'smartparens-config)
    (electric-pair-mode -1)
    (show-smartparens-global-mode 1)

    ;; Workaround for auto-paring issues for Rails and Django
    (eval-after-load 'web-mode
      (add-hook 'web-mode-hook
                '(lambda ()
                   (sp-local-pair 'web-mode "{" "}" :actions nil)
                   (sp-local-pair 'web-mode "<" ">" :actions nil))))

    ;; Hydra
    (use-package hydra
      :config
      (bind-key "C-M-s"
                (defhydra smartparens-hydra (:color pink)
                  "Smartparens"
                  ("d" sp-down-sexp "Down")
                  ("e" sp-up-sexp "Up")
                  ("u" sp-backward-up-sexp "Up")
                  ("a" sp-backward-down-sexp "Down")
                  ("f" sp-forward-sexp "Forward")
                  ("b" sp-backward-sexp "Backward")
                  ("s" sp-split-sexp "Split")
                  ("j" sp-join-sexp "Join")
                  ("k" sp-kill-sexp "Kill" :color blue)
                  ("q" nil "Quit" :color blue))
                smartparens-mode-map))

    ;; Pair Management
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

    ;; Markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*"
                     :wrap "C-*"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC"))
                     :skip-match 'sp--gfm-skip-asterisk)
      (sp-local-pair "**" "**")
      (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

    (defun sp--gfm-skip-asterisk (ms mb me)
      (save-excursion
        (goto-char mb)
        (save-match-data (looking-at "^\\* "))))

    ;; Org-mode
    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    (defun sp--org-skip-asterisk (ms mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))

    ;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">"))

    ;; lisp modes
    (sp-with-modes sp-lisp-modes
      (sp-local-pair "(" nil
                     :wrap "C-("
                     :pre-handlers '(my-add-space-before-sexp-insertion)
                     :post-handlers '(my-add-space-after-sexp-insertion)))

    (defun my-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun my-add-space-before-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (backward-char (length id))
          (when (or (eq (char-syntax (preceding-char)) ?w)
                    (and (looking-back (sp--get-closing-regexp))
                         (not (eq (char-syntax (preceding-char)) ?'))))
            (insert " ")))))

    ;; C
    (sp-with-modes '(malabar-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
    (sp-local-pair 'c-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))
    ;; C++
    (sp-with-modes '(malabar-mode c++-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
    (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                        ("* ||\n[i]" "RET")))))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
