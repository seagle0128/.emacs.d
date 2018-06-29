;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

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
;; Highlighting configurations.
;;

;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight symbols
(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook (prog-mode . symbol-overlay-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight indentions
(when (display-graphic-p)
  (use-package highlight-indent-guides
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive t)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((emacs-lisp-mode web-mode css-mode) . rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face (hl-todo ((t (:box t :bold t))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE" "WORKAROUND"))
    (cl-pushnew `(,keyword . "#cd5c5c") hl-todo-keyword-faces)))

;; Highlight uncommitted changes
(use-package diff-hl
  :defines desktop-minor-mode-table
  :commands diff-hl-magit-post-refresh
  :custom ((diff-hl-draw-borders nil)
           (fringes-outside-margins t)
           (fringe-mode '(4 . 8)))
  :custom-face
  (diff-hl-change ((t (:background "DeepSkyBlue"))))
  (diff-hl-delete ((t (:background "OrangeRed"))))
  (diff-hl-insert ((t (:background "YellowGreen"))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Highlight some operations
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

;; Flash the current line
(use-package nav-flash
  :defines compilation-highlight-overlay
  :functions windmove-do-window-select
  :preface
  (defun my-blink-cursor-maybe (orig-fn &rest args)
    "Blink current line if the window has moved."
    (let ((point (save-excursion (goto-char (window-start))
                                 (point-marker))))
      (apply orig-fn args)
      (unless (or (derived-mode-p 'term-mode)
                  (equal point
                         (save-excursion (goto-char (window-start))
                                         (point-marker))))
        (my-blink-cursor))))

  (defun my-blink-cursor (&rest _)
    "Blink current line using `nav-flash'."
    (interactive)
    (unless (minibufferp)
      (nav-flash-show)
      ;; only show in the current window
      (overlay-put compilation-highlight-overlay 'window (selected-window))))
  :hook ((imenu-after-jump switch-window-finish) . my-blink-cursor)
  :init
  ;; NOTE In :feature jump `recenter' is hooked to a bunch of jumping commands,
  ;; which will trigger nav-flash.
  (advice-add #'windmove-do-window-select :around #'my-blink-cursor-maybe)
  (advice-add #'other-window :around #'my-blink-cursor-maybe)
  (advice-add #'recenter :around #'my-blink-cursor-maybe))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
