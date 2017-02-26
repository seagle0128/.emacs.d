;; init-company.el --- Initialize company configurations.
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
;;             Company configurations.
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

(use-package company
  :defer t
  :diminish company-mode
  :bind (("M-/" . company-complete)
         ("C-c C-y" . company-yasnippet))
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; Display current candidate inline
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend))

  ;; Keybindings
  (bind-key "C-p" 'company-select-previous company-active-map)
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "TAB" 'company-complete-selection company-active-map)
  (bind-key "<tab>" 'company-complete-selection company-active-map)

  ;; Popup documentation for completion candidates
  (use-package company-quickhelp
    :defer t
    :if (display-graphic-p)
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :init (add-hook 'company-mode-hook 'company-quickhelp-mode))

  ;; Flx based fuzzy matching for company
  (use-package company-flx
    :defer t
    :init (add-hook 'company-mode-hook 'company-flx-mode))

  ;; Sort candidates using completion history
  (use-package company-statistics
    :defer t
    :init (add-hook 'company-mode-hook 'company-statistics-mode))

  ;; Company mode backend for C/C++ header files
  (use-package company-c-headers
    :defer t
    :init (push 'company-c-headers company-backends))

  ;; Complete for web,html,emmet,jade,slim modes
  (use-package company-web
    :defer t
    :init
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-web-jade)
    (add-to-list 'company-backends 'company-web-slim))

  ;; Company mode backend for shell functions
  (use-package company-shell
    :defer t
    :init
    (push 'company-shell company-backends)
    (push 'company-fish-shell company-backends))

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
