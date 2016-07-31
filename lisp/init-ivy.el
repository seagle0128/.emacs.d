;;; init-ivy.el --- Initialize ivy configurations.
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
;;             Ivy configurations.
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

(use-package counsel
  :defer t
  :diminish counsel-mode ivy-mode
  :defines magit-completing-read-function
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . ivy-recentf)
         ("C-." . counsel-imenu)
         ("C-S-t" . counsel-projectile-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h u" . counsel-unicode-char)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c s" . counsel-ag)
         ("C-c l" . counsel-locate)
         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :init
  (progn
    (add-hook 'after-init-hook 'counsel-mode)
    (add-hook 'after-init-hook 'ivy-mode)
    )
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 10)
    (setq ivy-count-format "(%d/%d) ")

    (setq ivy-re-builders-alist
          '((read-file-name-internal . ivy--regex-fuzzy)
            (t . ivy--regex-plus)))

    (setq swiper-action-recenter t)
    (setq counsel-find-file-at-point t)

    (setq projectile-completion-system 'ivy)
    (setq magit-completing-read-function 'ivy-completing-read)

    ;; Search and replace
    (define-key swiper-map (kbd "M-%") 'swiper-query-replace)

    ;; Search at point
    ;; "M-j": word-at-point
    ;; "M-n"/"C-w": symbol-at-point
    ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
    ;; and https://github.com/abo-abo/swiper/wikiiki/FAQ
    (define-key swiper-map (kbd "C-w")
      (lambda () (interactive) (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

    (use-package smex :defer t)
    (use-package ivy-hydra :defer t)

    (use-package counsel-projectile
      :defer t
      :bind (("C-S-t" . counsel-projectile-find-file))
      :init (setq projectile-switch-project-action 'counsel-projectile-find-file))
    ))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
