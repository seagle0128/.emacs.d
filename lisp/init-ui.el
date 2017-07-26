;; init-ui.el --- Initialize ui configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.2.0
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
(unless sys/mac-x-p
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Theme
(use-package monokai-theme
  :init (load-theme 'monokai t))

;; Modeline
(use-package spaceline-config
  :ensure spaceline
  :commands (spaceline-spacemacs-theme
             spaceline-info-mode)
  :init
  (setq powerline-default-separator (if sys/win32p 'arrow 'utf-8))
  (add-hook 'after-init-hook'(lambda () (spaceline-spacemacs-theme)))
  :config (with-eval-after-load 'info+ (spaceline-info-mode 1)))

;; Fonts
(use-package chinese-fonts-setup
  :commands cnfonts-enable
  :init (cnfonts-enable)
  :config
  (setq cnfonts-verbose nil)
  (setq cnfonts-save-current-profile nil)
  (setq cnfonts-use-face-font-rescale t)
  (setq cnfonts-profiles '("program" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program" . 4)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

(use-package nlinum
  :init
  (defun turn-on-nlinum ()
    "Turn on nlinum in small files."
    (interactive)
    (nlinum-mode (- (* 5000 80) (buffer-size))))

  (defun turn-off-nlinum ()
    "Turn off nlinum."
    (interactive)
    (nlinum-mode -1))

  (add-hook 'prog-mode-hook 'turn-on-nlinum)
  :config
  (setq nlinum-format "%4d ")

  ;; FIX: show-paren-mode erroneously highlights the left margin
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-10/msg01050.html
  (custom-set-faces '(linum ((t (:inherit default)))))

  ;; FIXME: refresh after exiting macrostep-mode
  (with-eval-after-load 'macrostep
    (add-hook 'macrostep-mode-hook
              '(lambda () (when nlinum-mode (turn-on-nlinum))))))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)

(use-package smooth-scrolling
  :init (add-hook 'after-init-hook 'smooth-scrolling-mode)
  :config (setq smooth-scroll-margin 0))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :preface
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  :init (add-hook 'after-init-hook 'display-time-mode))

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

;; Toggle fullscreen
(bind-keys ([(meta f11)] . toggle-frame-fullscreen)
           ([(meta return)] . toggle-frame-fullscreen)
           ([(meta shift return)] . toggle-frame-fullscreen))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
