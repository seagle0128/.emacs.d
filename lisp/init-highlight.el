;; init-highlight.el --- Initialize highlight configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.1.0
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
  :defer t
  :init (add-hook 'after-init-hook 'global-hl-line-mode))

;; Highlight symbol
(use-package highlight-symbol
  :defer t
  :diminish highlight-symbol-mode
  :bind (([C-f3] . highlight-symbol-at-point)
         ([f3] . highlight-symbol-next)
         ([S-f3] . highlight-symbol-prev)
         ([M-f3] . highlight-symbol-query-replace))
  :init
  (add-hook 'find-file-hook 'highlight-symbol-mode)
  (add-hook 'find-file-hook 'highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0))

;; Highlight indentions
(use-package indent-guide
  :defer t
  :diminish indent-guide-mode
  :init (add-hook 'after-init-hook 'indent-guide-global-mode))

;; Colorize color names in buffers
(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Color identifiers based on their names
(use-package color-identifiers-mode
  :defer t
  :diminish color-identifiers-mode)

;; Highlight TODO/FIXME/BUG
(use-package fic-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'fic-mode))

;; Highlight uncommitted changes
(use-package diff-hl
  :defer t
  :init
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :config
  (diff-hl-flydiff-mode 1)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (setq diff-hl-side 'right)
    (diff-hl-margin-mode 1))

  (eval-after-load 'magit
    '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (eval-after-load 'psvn
    '(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
       (diff-hl-update)))
  )

;; Highlight some operations
(use-package volatile-highlights
  :defer t
  :diminish volatile-highlights-mode
  :init (add-hook 'after-init-hook 'volatile-highlights-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face trailing space-before-tab indentation empty space-after-tab))
  ;; (setq whitespace-style '(face tabs empty trailing lines-tail))

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
        (whitespace-mode 1)))
  )

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
