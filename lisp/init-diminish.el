;; init-diminish.el --- Initialize diminish configurations.
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
;;             Diminish configurations.
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

(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "anaconda-mode" '(diminish 'anaconda-mode))
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode))
(eval-after-load "auto-indent-mode" '(diminish 'auto-indent-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "color-identifiers-mode" '(diminish 'color-identifiers-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-current-column-mode))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "robe" '(diminish 'robe-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "whitespace" '(diminish 'whitespace-mode))
(eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))
(eval-after-load "yard-mode" '(diminish 'yard-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))

(setq-default anzu-mode-lighter nil)
(setq-default back-button-mode-lighter nil)
(setq-default ctags-update-lighter nil)
(setq-default flycheck-mode-line nil)
(setq-default projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

(provide 'init-diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-diminish.el ends here
