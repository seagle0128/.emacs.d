;; init-window.el --- Initialize window configurations.
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
;;             Window configurations.
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

(windmove-default-keybindings)
(winner-mode 1)

;; Switch window
(use-package switch-window
  :defer t
  :bind ("C-x o" . switch-window))

;; Zoom window
(use-package zoom-window
  :defer t
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))

;; Popwin
(use-package popwin
  :defer t
  :commands popwin-mode
  :init (add-hook 'after-init-hook 'popwin-mode)
  :config
  (progn
    ;; (global-set-key (kbd "C-z") popwin:keymap)

    ;; Support browse-kill-ring
    (eval-after-load 'browse-kill-ring
      '(progn
         (defun popwin-bkr:update-window-reference ()
           (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))

         (add-hook 'popwin:after-popup-hook 'popwin-bkr:update-window-reference)

         (push "*Kill Ring*" popwin:special-display-config)))
    ))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
