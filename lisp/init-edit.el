;; init-edit.el --- Initialize edit configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.0.0
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

;; Rectangle
;; (setq cua-enable-cua-keys nil)           ;; don't add C-x,C-c,C-v
;; (cua-mode t)                             ;; for rectangles, CUA is nice

(use-package hydra
  :config
  (progn
    (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :post (deactivate-mark))
      "
  ^_k_^    _d_elete    _s_tring     |\\     _,,,--,,
_h_   _l_  _o_k        _y_ank       /,`.-'`'   ._  \-;;,-
  ^_j_^    _n_ew-copy  _r_eset     |,4-  ) )_   .;.(  `'-'
^^^^       _e_xchange  _u_ndo     '---''(,/.,)-'(,\,)
^^^^       _p_aste     _q_uit
"
      ("h" backward-char nil)
      ("l" forward-char nil)
      ("k" previous-line nil)
      ("j" next-line nil)
      ("e" exchange-point-and-mark nil)
      ("n" copy-rectangle-as-kill nil)
      ("d" delete-rectangle nil)
      ("r" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)) nil)
      ("y" yank-rectangle nil)
      ("u" undo nil)
      ("s" string-rectangle nil)
      ("p" kill-rectangle nil)
      ("o" nil nil)
      ("q" nil nil)
      ("C-<return>" nil nil))
    (global-set-key (kbd "C-<return>") 'hydra-rectangle/body)
    ))

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
  :init (progn
          (setq auto-indent-assign-indent-level-variables nil)
          (setq auto-indent-indent-style 'conservative)
          (add-hook 'after-init-hook 'auto-indent-global-mode)))

;; Anzu mode
(use-package anzu
  :defer t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :init (add-hook 'after-init-hook 'global-anzu-mode))

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
         ("M-<down-mouse-1>" . mc/add-cursor-on-click))
  :config
  (progn
    (use-package hydra
      :config
      (progn
        (defhydra multiple-cursors-hydra (:color pink :hint nil)
          "
     ^Up^            ^Down^        ^Other^
 ----------------------------------------------
 [_p_]   Next    [_n_]   Next    [_l_] Edit lines
 [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
 [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
 ^ ^             ^ ^             [_q_] Quit
"
          ("l" mc/edit-lines :exit t)
          ("a" mc/mark-all-like-this :exit t)
          ("n" mc/mark-next-like-this)
          ("N" mc/skip-to-next-like-this)
          ("M-n" mc/unmark-next-like-this)
          ("p" mc/mark-previous-like-this)
          ("P" mc/skip-to-previous-like-this)
          ("M-p" mc/unmark-previous-like-this)
          ("r" mc/mark-all-in-region-regexp :exit t)
          ("<mouse-1>" mc/add-cursor-on-click)
          ("<down-mouse-1>" ignore)
          ("<drag-mouse-1>" ignore)
          ("q" nil))
        (global-set-key (kbd "C-S-c") 'multiple-cursors-hydra/body)
        ))))

;; Expand region
(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; Subword and Superword
(use-package subword
  :defer t
  :diminish subword-mode
  :init (add-hook 'prog-mode-hook 'subword-mode))

;; Origami code floding
(use-package origami
  :defer t
  :init (add-hook 'after-init-hook 'global-origami-mode)
  :config
  (use-package hydra
    :config
    (progn
      (defhydra hydra-folding (:color pink)
        "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll     _q_uit
"
        ("o" origami-open-node)
        ("c" origami-close-node)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("f" origami-forward-toggle-node)
        ("a" origami-toggle-all-nodes)
        ("q" nil))
      (global-set-key (kbd "C-S-f") 'hydra-folding/body)
      )))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
