;; init-highlight.el --- Initialize highlight configurations.
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

;; Highlight line
(global-hl-line-mode 1)

;; Highlight symbol
(add-hook 'find-file-hook 'highlight-symbol-mode)
(add-hook 'find-file-hook 'highlight-symbol-nav-mode 1)
(setq highlight-symbol-idle-delay 0)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; Highlight indentions
(defvar web-mode-html-offset 2)         ; Workaround. Fix void var issue.
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)

;; Rainbow
(add-hook 'prog-mode-hook 'rainbow-mode)

;; Highlight delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight identifiers
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; Highlight TODO/FIXME/BUG
(add-hook 'prog-mode-hook 'fic-mode)

;; Highlight uncommitted changes
(global-diff-hl-mode t)

;; Highlight some operations
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Whitespace
(add-hook 'prog-mode-hook 'whitespace-mode t)
(setq whitespace-line-column fill-column) ;; limit line length
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(face trailing space-before-tab indentation empty space-after-tab))
;; (setq whitespace-style '(face tabs empty trailing lines-tail))

;;; advice for whitespace-mode conflict with popup
(defvar my-prev-whitespace-mode nil)
(make-variable-buffer-local 'my-prev-whitespace-mode)

(defadvice popup-draw (before my-turn-off-whitespace activate compile)
  "Turn off whitespace mode before showing autocomplete box."
  (make-local-variable 'my-prev-whitespace-mode)
  (if whitespace-mode
      (progn
        (setq my-prev-whitespace-mode t)
        (whitespace-mode -1))
    (setq my-prev-whitespace-mode nil)))

(defadvice popup-delete (after my-restore-whitespace activate compile)
  "Restore previous whitespace mode when deleting autocomplete box."
  (if my-prev-whitespace-mode
      (whitespace-mode 1)))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-highlight.el ends here
