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
  :defines all-the-icons-icon-alist
  :functions (all-the-icons-icon-for-buffer
              all-the-icons-icon-family)
  :commands (ibuffer-current-buffer
             ibuffer-find-file
             ibuffer-do-sort-by-alphabetic)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

  ;; Display buffer icons on GUI
  (when (display-graphic-p)
    (define-ibuffer-column icon (:name " ")
      (let ((icon (if (and buffer-file-name
                           (all-the-icons-match-to-alist buffer-file-name
                                                         all-the-icons-icon-alist))
                      (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
                                                   :height 0.9 :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
          icon)))

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
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.1
                                      :height 1.1)
               " ")
            "Project: "))))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
