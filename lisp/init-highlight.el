;; init-highlight.el --- Initialize highlight configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.3.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Highlight configurations.
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

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :init (add-hook 'after-init-hook #'global-hl-line-mode))

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
  :init (add-hook 'prog-mode-hook #'symbol-overlay-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :init (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight indentions
(use-package highlight-indent-guides
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'rainbow-mode))
  (with-eval-after-load 'css-mode
    (add-hook 'css-mode-hook #'rainbow-mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package fic-mode
  :init (add-hook 'prog-mode-hook #'fic-mode)
  :config
  (setq fic-highlighted-words '("TODO" "FIXME" "FIX" "BUG" "ISSUE" "DEFECT" "WORKAROUND"))
  (setq fic-activated-faces '(font-lock-comment-face))
  (set-face-attribute 'fic-face nil :box t :background (face-background 'default))
  (set-face-background 'fic-author-face (face-background 'default)))

;; Highlight uncommitted changes
(use-package diff-hl
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  :config
  (diff-hl-flydiff-mode 1)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))

  ;; Avoid restoring `diff-hl-margin-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(diff-hl-margin-mode nil)))

  ;; Integration with magit and psvn
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (with-eval-after-load 'psvn
    (defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
      (diff-hl-update))))

;; Highlight some operations
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init (add-hook 'after-init-hook #'volatile-highlights-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook outline-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
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

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
