;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2009-2020 Vincent Zhang

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
;; Flycheck configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-funcs)

(use-package flycheck
  :diminish
  ;; FIXME: Fix args-out-of-range error
  ;; @see https://github.com/flycheck/flycheck/issues/1677
  ;; :hook (after-init . global-flycheck-mode)
  :hook ((prog-mode markdown-mode) . (lambda ()
                                       (unless (centaur-timemachine-buffer-p)
                                         (flycheck-mode 1))))
  :config
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode org-mode
              diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-fringe)

  ;; Prettify fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
            :hook (flycheck-mode . flycheck-posframe-mode)
            :init (setq flycheck-posframe-border-width 1
                        flycheck-posframe-inhibit-functions
                        '((lambda (&rest _) (bound-and-true-p company-backend)))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
