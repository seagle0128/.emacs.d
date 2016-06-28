;;; init-ido.el --- Initialize ido configurations.
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
;;             Ido configurations.
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

(eval-when-compile (require 'ido))
(eval-when-compile (require 'recentf))

(ido-mode 1)
(ido-everywhere 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)

(eval-after-load 'recentf
  '(progn
     (defun ido-recentf-find-file ()
       "Find a recent file using ido."
       (interactive)
       (let ((file (ido-completing-read "Choose recent file: "
                                        (-map 'abbreviate-file-name recentf-list)
                                        nil t)))
         (when file
           (find-file file))))
     (global-set-key (kbd "C-x C-r") 'ido-recentf-find-file)
     ))

(use-package ido-ubiquitous
  :defer t
  :init (add-hook 'after-init-hook 'ido-ubiquitous-mode))

(use-package ido-at-point
  :defer t
  :init (add-hook 'after-init-hook 'ido-at-point-mode))

(use-package ido-complete-space-or-hyphen
  :defer t
  :init (add-hook 'after-init-hook 'ido-complete-space-or-hyphen-enable))

(use-package ido-sort-mtime
  :defer t
  :init (add-hook 'after-init-hook 'ido-sort-mtime-mode))

(use-package ido-vertical-mode
  :defer t
  :init
  (progn
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
    (setq ido-vertical-show-count t)
    (add-hook 'after-init-hook 'ido-vertical-mode)
    ))

(use-package flx-ido
  :defer t
  :init (add-hook 'after-init-hook 'flx-ido-mode))

(use-package ido-load-library
  :defer t
  :init (defalias 'load-library 'ido-load-library))

(use-package imenus
  :defer t
  :bind ("C-." . imenus))

(use-package smex
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command)))

(use-package ido-occur
  :defer t
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (progn
                 (defun ido-occur-at-point ()
                   "Open ido-occur at point."
                   (interactive)
                   (ido-occur (symbol-name (symbol-at-point))))

                 (global-set-key (kbd "C-o") 'ido-occur-at-point)

                 (defun ido-occur-from-isearch ()
                   "Open ido-occur from isearch."
                   (interactive)
                   (ido-occur (if isearch-regexp
                                  isearch-string
                                (regexp-quote isearch-string))))

                 (define-key isearch-mode-map (kbd "C-o") 'ido-occur-from-isearch)
                 ))))

(provide 'init-ido)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ido.el ends here
