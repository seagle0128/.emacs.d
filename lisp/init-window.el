;; init-window.el --- Initialize window configurations.
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

;; Directional window-selection routines
(use-package windmove
  :defer t
  :init (add-hook 'window-setup-hook 'windmove-default-keybindings))

;; Restore old window configurations
(use-package winner
  :defer t
  :init (add-hook 'window-setup-hook 'winner-mode))

;; Quickly switch windows
(use-package ace-window
  :defer t
  :bind ("C-x o" . ace-window))

;; Zoom window like tmux
(use-package zoom-window
  :defer t
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))

;; Popup Window Manager
(use-package popwin
  :defer t
  :commands popwin-mode
  :init (add-hook 'after-init-hook 'popwin-mode)
  :config
  (progn
    (global-set-key (kbd "C-z") popwin:keymap)

    ;; Support browse-kill-ring
    (eval-after-load 'browse-kill-ring
      '(progn
         (defun popwin-bkr:update-window-reference ()
           (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))

         (add-hook 'popwin:after-popup-hook 'popwin-bkr:update-window-reference)

         (push "*Kill Ring*" popwin:special-display-config)))
    ))

;; Minimal window layout manager

(use-package wconf
  :defer t
  :bind (("C-c w c" . wconf-create)
         ("C-c w k" . wconf-kill)
         ("C-c w l" . wconf-load)
         ("C-c w m" . wconf-rename)
         ("C-c w n" . wconf-use-next)
         ("C-c w p" . wconf-use-previous)
         ("C-c w s" . wconf-store)
         ("C-c w S" . wconf-store-all)
         ("C-c w r" . wconf-restore)
         ("C-c w R" . wconf-restore-all)
         ("C-c w w" . wconf-switch-to-config)
         ("C-c w v" . wconf-save))
  :init
  (progn
    (set wconf-file (expand-file-name "wconf-window-configs.el"
                                      user-emacs-directory))

    (defun create-wconf-file ()
      (unless (file-exists-p wconf-file)
        (write-region "" nil wconf-file)
        (wconf-create)
        (wconf-store-all)
        (wconf-save)))

    (add-hook 'desktop-after-read-hook
              '(lambda ()
                 (create-wconf-file)
                 (wconf-load)
                 (wconf-switch-to-config 0)))

    (add-hook 'kill-emacs-hook
              '(lambda ()
                 (create-wconf-file)
                 (wconf-store-all)
                 (wconf-save)))
    ))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
