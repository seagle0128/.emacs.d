;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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
;; IBuffer configurations.
;;

;;; Code:

(use-package ibuffer
  :ensure nil
  :commands (ibuffer-current-buffer ibuffer-find-file ibuffer-do-sort-by-alphabetic)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (success bold)))

  ;; Display buffer icons on GUI
  (when (and (display-graphic-p)
             (featurep 'all-the-icons))
    (define-ibuffer-column icon (:name " " :inline t)
      (let ((icon (all-the-icons-icon-for-file (buffer-name))))
        (unless (symbolp icon)
          (propertize icon
                      'face `(
                              :height 1.1
                              :family ,(all-the-icons-icon-family icon)
                              :inherit
                              )))))

    (setq ibuffer-formats '((mark modified read-only locked
                                  " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  :config
  (with-eval-after-load 'counsel
    (defalias 'ibuffer-find-file 'counsel-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (and (display-graphic-p)
                   (featurep 'all-the-icons))
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: "))))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
