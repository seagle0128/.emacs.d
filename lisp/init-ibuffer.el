;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Vincent Zhang

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
;; IBuffer configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (ibuffer-human-readable-size t)
  :bind ("C-x C-b" . ibuffer))

;; Group ibuffer's list by VC project
(use-package ibuffer-vc
  :commands (ibuffer-vc-set-filter-groups-by-vc-root
             ibuffer-do-sort-by-vc-status)
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-vc-status))))
  :config
  (with-no-warnings
    (when (icons-displayable-p)
      (defun my/ibuffer-vc-generate-filter-groups-by-vc-root ()
        "Create a set of ibuffer filter groups based on the vc root dirs of buffers."
        (let ((roots (seq-uniq
                      (delq nil (mapcar 'ibuffer-vc-root (buffer-list))))))
          (mapcar (lambda (vc-root)
                    (cons (format "%s: %s"
                                  (nerd-icons-octicon "nf-oct-repo"
                                                      :height 1.2
                                                      :face ibuffer-filter-group-name-face)
                                  (cdr vc-root))
                          `((vc-root . ,vc-root))))
                  roots)))
      (advice-add #'ibuffer-vc-generate-filter-groups-by-vc-root
                  :override #'my/ibuffer-vc-generate-filter-groups-by-vc-root))))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :custom
  (nerd-icons-ibuffer-icon centaur-icon)
  ;; display vc-status, which needs `ibuffer-vc'
  (nerd-icons-ibuffer-formats '((mark modified read-only locked vc-status-mini
                                      " " (icon 2 2) (name 18 18 :left :elide)
                                      " " (size-h 9 -1 :right)
                                      " " (mode+ 16 16 :left :elide)
                                      " " (vc-status 16 16 :left)
                                      " " filename-and-process+)
                                (mark " " (name 16 -1) " " filename)))
  :hook ibuffer-mode)

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
