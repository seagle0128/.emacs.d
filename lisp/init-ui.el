;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.3.0
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

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Logo
(setq fancy-splash-image my-logo)

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Menu/Tool/Scroll bars
(unless sys/mac-x-p (menu-bar-mode -1))
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Theme
(defun is-doom-theme-p (theme)
  "Check whether the THEME is a doom theme. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name theme)))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; Modeline
(if (is-doom-theme-p my-theme)
    (progn
      (use-package doom-modeline
        :ensure powerline
        :demand
        :config (add-hook 'after-init-hook
                          (lambda ()
                            (setq-default mode-line-format (doom-mode-line)))))
      (use-package hide-mode-line
        :init
        (dolist (hook '(completion-list-mode-hook
                        eshell-mode-hook shell-mode-hook term-mode-hook
                        magit-mode-hook magit-diff-mode-hook magit-log-mode-hook magit-popup-mode-hook
                        helpful-mode-hook treemacs-mode-hook))
          (add-hook hook #'hide-mode-line-mode))))
  (use-package spaceline-config
    :ensure spaceline
    :commands spaceline-spacemacs-theme1
    :init
    (setq powerline-default-separator (if window-system 'arrow 'utf-8))
    (setq powerline-image-apple-rgb sys/mac-x-p)
    (add-hook 'after-init-hook #'spaceline-spacemacs-theme)
    :config
    (setq spaceline-pre-hook #'powerline-reset) ; For changing themes
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)))

;; Color theme
(cond
 ((eq my-theme 'default)
  (use-package monokai-theme
    :init (load-theme 'monokai t)))

 ((eq my-theme 'dark)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-dark t)))

 ((eq my-theme 'light)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-light t)))

 ((eq my-theme 'daylight)
  (use-package leuven-theme
    :init (load-theme 'leuven t)))

 ((is-doom-theme-p my-theme)
  (use-package doom-themes
    :preface (defvar region-fg nil)
    :init
    (if (eq my-theme 'doom)
        (load-theme 'doom-one t)
      (load-theme my-theme t))
    :config
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)

    (use-package solaire-mode
      :init
      (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
      (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
      (add-hook 'after-revert-hook #'turn-on-solaire-mode)
      (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
      (solaire-mode-swap-bg))))

 (t
  (ignore-errors (load-theme my-theme t))))

;; Fonts
(use-package cnfonts
  :init
  (add-hook 'after-init-hook #'cnfonts-enable)
  :config
  (setq cnfonts-keep-frame-size nil)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq cnfonts-keep-frame-size t)))
  ;; `cnfonts' has issue on Emacs 26
  (balance-windows)

  (setq cnfonts-use-cache t)
  (setq cnfonts-profiles
        '("program1" "program2" "program3" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program1" . 4)
                                  ("program2" . 5)
                                  ("program3" . 3)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :init (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package linum-off
    :demand
    :init (add-hook 'after-init-hook #'global-linum-mode)
    :config
    (setq linum-format "%4d ")
    ;; Highlight current line number
    (use-package hlinum
      :init (add-hook 'global-linum-mode-hook #'hlinum-activate)
      :config
      (setq linum-highlight-in-all-buffersp t)
      (add-hook 'after-load-theme-hook
                (lambda ()
                  (set-face-attribute 'linum-highlight-face
                                      nil
                                      :inherit 'default
                                      :background (face-background 'default)
                                      :foreground (face-foreground 'default)))))))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :preface
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  :init (add-hook 'after-init-hook #'display-time-mode))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(size-indication-mode 1)
;; (blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Toggle fullscreen
(bind-keys ([(control f11)] . toggle-frame-fullscreen)
           ([(control super f)] . toggle-frame-fullscreen) ; Compatible with macOS
           ([(super return)] . toggle-frame-fullscreen)
           ([(meta shift return)] . toggle-frame-fullscreen))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
