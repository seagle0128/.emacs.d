;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

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
;; Flycheck configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Set fringe style
  (setq flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :hook (flycheck-mode . flycheck-posframe-mode)
            :config
            (add-to-list 'flycheck-posframe-inhibit-functions
                         #'(lambda () (bound-and-true-p company-backend)))

            (with-no-warnings
              (defun my-flycheck-posframe-show-posframe (errors)
                "Display ERRORS, using posframe.el library."
                (flycheck-posframe-hide-posframe)
                (when (and errors
                           (not (run-hook-with-args-until-success 'flycheck-posframe-inhibit-functions)))
                  (let ((poshandler (intern (format "posframe-poshandler-%s" flycheck-posframe-position))))
                    (unless (functionp poshandler)
                      (setq poshandler nil))
                    (posframe-show
                     flycheck-posframe-buffer
                     :string (flycheck-posframe-format-errors errors)
                     :background-color (face-background 'flycheck-posframe-background-face nil t)
                     :position (point)
                     :internal-border-width flycheck-posframe-border-width
                     :internal-border-color (face-foreground'flycheck-posframe-border-face nil t)
                     :poshandler poshandler))
                  (dolist (hook flycheck-posframe-hide-posframe-hooks)
                    (add-hook hook #'flycheck-posframe-hide-posframe nil t))))
              (advice-add #'flycheck-posframe-show-posframe :override #'my-flycheck-posframe-show-posframe)))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
