;;; init-ido.el --- Initialize ido configurations.
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

(use-package ido
  :ensure nil
  :init (add-hook 'after-init-hook 'ido-mode)
  :config
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
       (bind-key "C-x C-r" 'ido-recentf-find-file)
       ))

  (use-package ido-ubiquitous :init (ido-ubiquitous-mode 1))
  (use-package ido-at-point :init (ido-at-point-mode 1))
  (use-package ido-complete-space-or-hyphen :init (ido-complete-space-or-hyphen-enable))
  (use-package ido-sort-mtime :init (ido-sort-mtime-mode 1))
  (use-package flx-ido :init (flx-ido-mode 1))

  (use-package ido-vertical-mode
    :init
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
    (setq ido-vertical-show-count t))

  (use-package ido-load-library
    :init (defalias 'load-library 'ido-load-library))

  (use-package imenus :bind ("C-." . imenus))

  (use-package smex
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)
           ("C-c M-x" . execute-extended-command)))

  (use-package ido-occur
    :bind (("C-c o" . ido-occur)
           ("C-c O" . ido-occur-at-point)
           :isearch-mode-map
           ("C-o" . ido-occur-from-isearch)))

  (use-package ggtags
    :diminish ggtags-mode
    :init (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                        (ggtags-mode 1)))))
  )

(provide 'init-ido)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ido.el ends here
