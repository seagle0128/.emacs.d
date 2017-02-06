;; init-ui.el --- Initialize ui configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.1.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             UI configurations.
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

(require 'init-const)

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Menu/Tool/Scroll bars
(when (and (fboundp 'menu-bar-mode) (not sys/mac-x-p)) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Modeline configuration
(use-package spaceline-config
  :ensure spaceline
  :defer t
  :commands spaceline-spacemacs-theme spaceline-emacs-theme spaceline-info-mode spaceline-helm-mode
  :init (add-hook 'after-init-hook
                  '(lambda ()
                     (spaceline-spacemacs-theme)
                     (eval-after-load 'info+ '(spaceline-info-mode 1))
                     (eval-after-load 'helm '(spaceline-helm-mode 1)))))

;; Color theme
(use-package monokai-theme
  :defer t
  :init
  (progn
    (defun load-monokai-theme()
      "Load Monokai theme and set new tooltip background color."
      (load-theme 'monokai t)
      (set-face-background 'tooltip "#FEFBD5")
      (when (boundp 'pos-tip-background-color)
        (setq pos-tip-background-color "#FEFBD5")))

    (add-hook 'after-init-hook 'load-monokai-theme)
    ))

;; Fonts
(use-package chinese-fonts-setup
  :defer t
  :commands chinese-fonts-setup-enable
  :defines cfs--current-profile-name
  :init (add-hook 'emacs-startup-hook
                  '(lambda ()
                     (setq cfs-verbose nil)
                     (setq cfs-save-current-profile nil)
                     (chinese-fonts-setup-enable)))
  :config
  (progn
    (setq cfs-profiles
          '("program" "org-mode" "read-book"))
    (setq cfs--current-profile-name "program")
    (when sys/mac-x-p
      (setq cfs--profiles-steps '(("program" . 5)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))
    ))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)
(use-package linum-off :config (global-linum-mode 1))

;; Mouse & Smooth Scroll
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)

(use-package smooth-scrolling
  :defer t
  :init (add-hook 'after-init-hook 'smooth-scrolling-mode)
  :config (setq smooth-scroll-margin 0))

;; Display Time
(use-package time
  :defer t
  :init (add-hook 'window-setup-hook 'display-time-mode)
  :config (progn (setq display-time-24hr-format t)
                 (setq display-time-day-and-date t)))
;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq-default ns-pop-up-frames nil)     ; Don't open a file in a new frame
(size-indication-mode 1)
;; (blink-cursor-mode -1)
(show-paren-mode 1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Emacs startup screen
(use-package dashboard
  :defer t
  :after projectile
  :diminish page-break-lines-mode
  :init (dashboard-setup-startup-hook)
  :config
  (when projectile-mode
    (add-to-list 'dashboard-items '(projects . 5))))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
