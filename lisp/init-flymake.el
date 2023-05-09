;; init-flymake.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2009-2023 Vincent Zhang

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
;; Flymake configurations.
;;

;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package flymake-elisp-config
  :hook ((after-init . flymake-elisp-config-global-mode)
         (flymake-elisp-config-global-mode . flymake-elisp-config-auto-mode)))

(use-package flymake-diagnostic-at-point
  :commands flymake-diagnostic-at-point-mode
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (when (and (childframe-workable-p)
             (require 'posframe nil t))
    (defvar flymake-posframe-buffer " *flymake-posframe-buffer*"
      "Name of the flymake posframe buffer.")

    (defvar flymake-posframe-last-position nil
      "Last position for which a flycheck posframe was displayed.")

    (defun flymake-posframe-check-position ()
      "Update `flymake-posframe-last-position', returning t if there was no change."
      (equal flymake-posframe-last-position
             (setq flymake-posframe-last-position
                   (list (current-buffer) (buffer-modified-tick) (point)))))

    (defun flymake-posframe-hidehandler (_info)
      "Hide posframe if position has changed since last display."
      (not (flymake-posframe-check-position)))

    (defun flymake-diagnostic-at-point-display-posframe (text)
      "Display the flymake diagnostic TEXT inside a child frame."
      (flycheck-posframe-check-position)
      (posframe-show flymake-posframe-buffer
                     :string (concat flymake-diagnostic-at-point-error-prefix text)
	                 :left-fringe 4
	                 :right-fringe 4
                     :max-width (round (* (frame-width) 0.62))
                     :max-height (round (* (frame-height) 0.62))
                     :internal-border-width 1
                     :internal-border-color (face-background 'posframe-border nil t)
                     :background-color (face-background 'tooltip nil t)
                     :hidehandler #'flymake-posframe-hidehandler))

    (defun flymake-posframe-hide ()
      (posframe-hide flymake-posframe-buffer)
      (dolist (hook flymake-posframe-hide-posframe-hooks)
        (remove-hook hook #'flymake-posframe-hide t)))

    ;; (dolist (hook flymake-posframe-hide-posframe-hooks)
    ;;   (add-hook hook #'flymake-posframe-hide nil t))

    (setq flymake-diagnostic-at-point-display-diagnostic-function
          #'flymake-diagnostic-at-point-display-posframe)))

(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
