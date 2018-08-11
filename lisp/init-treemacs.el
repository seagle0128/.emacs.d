;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

;; Require >=25.2
(when (or (>= emacs-major-version 26)
          (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  ;; A tree layout file explorer
  (use-package treemacs
    :defines winum-keymap
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f8]        . treemacs)
           ("M-9"       . treemacs-select-window)
           ("C-x 1"     . treemacs-delete-other-windows)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t B"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :init
    (with-eval-after-load 'winum
      (bind-key (kbd "M-9") #'treemacs-select-window winum-keymap))
    :config
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           t
          treemacs-silent-refresh             t
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (if (fboundp 'define-fringe-bitmap)
        (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
          (vector #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111
                  #b00000111111))))

  ;; Projectile integration for treemacs
  (use-package treemacs-projectile
    :after projectile
    :bind (([M-f8] . treemacs-projectile)
           :map projectile-command-map
           ("h" . treemacs-projectile)))
  )

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
