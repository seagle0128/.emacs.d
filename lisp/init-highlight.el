;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

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
;; Highlighting configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :functions childframe-workable-p
  :custom-face
  (show-paren-match ((((class color) (background light))
                      (:box (:line-width (-1 . -1) :color "gray73")))
                     (((class color) (background dark))
                      (:box (:line-width (-1 . -1) :color "gray56")))))
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (if emacs/>=29p
      (setq blink-matching-paren-highlight-offscreen t
            show-paren-context-when-offscreen
            (if (childframe-workable-p) 'child-frame 'overlay))
    (with-no-warnings
      ;; Display matching line for off-screen paren.
      (defun display-line-overlay (pos str &optional face)
        "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
        (let ((ol (save-excursion
                    (goto-char pos)
                    (make-overlay (line-beginning-position)
                                  (line-end-position)))))
          (overlay-put ol 'display str)
          (overlay-put ol 'face
                       (or face '(:inherit highlight)))
          ol))

      (defvar-local show-paren--off-screen-overlay nil)
      (defun show-paren-off-screen (&rest _args)
        "Display matching line for off-screen paren."
        (when (overlayp show-paren--off-screen-overlay)
          (delete-overlay show-paren--off-screen-overlay))
        ;; Check if it's appropriate to show match info,
        (when (and (overlay-buffer show-paren--overlay)
                   (not (or cursor-in-echo-area
                            executing-kbd-macro
                            noninteractive
                            (minibufferp)
                            this-command))
                   (and (not (bobp))
                        (memq (char-syntax (char-before)) '(?\) ?\$)))
                   (= 1 (logand 1 (- (point)
                                     (save-excursion
                                       (forward-char -1)
                                       (skip-syntax-backward "/\\")
                                       (point))))))
          ;; Rebind `minibuffer-message' called by `blink-matching-open'
          ;; to handle the overlay display.
          (cl-letf (((symbol-function #'minibuffer-message)
                     (lambda (msg &rest args)
                       (let ((msg (apply #'format-message msg args)))
                         (setq show-paren--off-screen-overlay
                               (display-line-overlay
                                (window-start) msg ))))))
            (blink-matching-open))))
      (advice-add #'show-paren-function :after #'show-paren-off-screen))))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :custom-face
  (symbol-overlay-default-face ((t (:inherit region :background unspecified :foreground unspecified))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :bind (("M-i"  . symbol-overlay-put)
         ("M-n"  . symbol-overlay-jump-next)
         ("M-p"  . symbol-overlay-jump-prev)
         ("M-N"  . symbol-overlay-switch-forward)
         ("M-P"  . symbol-overlay-switch-backward)
         ("M-C"  . symbol-overlay-remove-all)
         ([M-f2] . symbol-overlay-mode)
         ([M-f3] . symbol-overlay-put)
         ([M-f4] . symbol-overlay-remove-all))
  :bind-keymap ("M-s s" . symbol-overlay-map)
  :hook (((prog-mode yaml-mode yaml-ts-mode) . symbol-overlay-mode)
         (iedit-mode     . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.3)
  :config
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode 'yaml-ts-mode)
        (symbol-overlay-mode 1)))

    (advice-add #'activate-mark :after #'turn-off-symbol-overlay)
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)
    (advice-add #'easy-kill :after #'turn-off-symbol-overlay)
    (advice-add #'easy-kill-destroy-candidate :after #'turn-on-symbol-overlay)))

;; Mark occurrences of current region (selection)
(use-package region-occurrences-highlighter
  :diminish
  :bind (:map region-occurrences-highlighter-nav-mode-map
         ("M-n" . region-occurrences-highlighter-next)
         ("M-p" . region-occurrences-highlighter-prev))
  :hook (after-init . global-region-occurrences-highlighter-mode))

;; Highlight indentions
(use-package indent-bars
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.225))
  (indent-bars-treesit-support centaur-tree-sitter)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				                       if_statement with_statement while_statement)))
  :hook ((prog-mode yaml-mode) . indent-bars-mode)
  :config (require 'indent-bars-ts))

;; Colorize color names in buffers
(use-package colorful-mode
  :diminish
  :hook (after-init . global-colorful-mode)
  :init (setq colorful-use-prefix t)
  :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :autoload hl-todo-flymake hl-todo-search-and-highlight
  :functions rg rg-read-files rg-project
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :bind (:map hl-todo-mode-map
         ([C-f3]    . hl-todo-occur)
         ("C-c t o" . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t i" . hl-todo-insert)
         ("C-c t r" . hl-todo-rg-project)
         ("C-c t R" . hl-todo-rg))
  :hook (after-init . global-hl-todo-mode)
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#e45649")))
  (dolist (keyword '("TRICK" "WORKAROUND"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#d0bf8f")))
  (dolist (keyword '("DEBUG" "STUB"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#7cb8bb")))

  ;; Integrate into flymake
  (with-eval-after-load 'flymake
    (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

  ;; Integrate into magit
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook
              #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook
              #'hl-todo-search-and-highlight t))

  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (error "`rg' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))

  (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (error "`rg' is not installed"))
    (rg-project (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "everything")))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom (diff-hl-draw-borders nil)
  :autoload diff-hl-flydiff-mode
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/linuxp #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; Pulse current line
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window switch-to-buffer
                   aw-select toggle-window-split
                   windmove-do-window-select
                   pager-page-down pager-page-up
                   treemacs-select-window))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))

;; Pulse modified region
(use-package goggles
  :diminish
  :hook ((prog-mode text-mode conf-mode) . goggles-mode))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
