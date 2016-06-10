;; init-ui.el --- Initialize ui configurations.
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

;; Menu/Tool/Scroll bars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Spaceline
(use-package spaceline
  :defer t
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (require 'spaceline-config)
               (spaceline-emacs-theme)
               (spaceline-helm-mode 1))))

;; Color theme
;; DO NOT use use-package to load themes
(unless (package-installed-p 'monokai-theme)
  (package-install 'monokai-theme))
(add-hook 'after-init-hook
          '(lambda()
             (load-theme 'monokai t)))

;; Fonts
(use-package chinese-fonts-setup
  :defines cfs--current-profile-name
  :config
  (progn
    (setq cfs-profiles
          '("program" "org-mode" "read-book"))
    (setq cfs--current-profile-name "program")
    ;; (if sys/macp
    ;;     (setq cfs--fontsize-steps '(6 6 8)))
    ))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)
;; (global-linum-mode 1)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;; Mouse & Smooth Scroll
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)

(use-package smooth-scrolling
  :init (setq-default smooth-scroll-margin 0)
  :config (smooth-scrolling-mode 1))

;; Display Time
(use-package time
  :defer t
  :if (not (display-graphic-p))
  :init
  (progn
    (setq display-time-24hr-format t)
    (setq display-time-day-and-date t)
    (display-time-mode 1)
    ))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
;; (setq visible-bell t)
(setq-default ns-pop-up-frames nil)             ; Don't open a file in a new frame
(size-indication-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(setq track-eol t)                         ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(ansi-color-for-comint-mode-on)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
