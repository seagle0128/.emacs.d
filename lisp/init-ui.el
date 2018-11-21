;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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
;; Visual (UI) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Logo
(setq fancy-splash-image centaur-logo)

;; Title
(setq frame-title-format
      '("Centaur Emacs - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Menu/Tool/Scroll bars
(unless emacs/>=27p        ; Move to early init-file in 27
  (unless sys/mac-x-p (menu-bar-mode -1))
  (and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
  (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

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
(defun mode-line-height ()
  "Get current height of mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(if (is-doom-theme-p centaur-theme)
    (use-package doom-modeline
      :hook ((after-init . doom-modeline-init)
             (dashboard-mode . doom-modeline-set-project-modeline)))
  (use-package telephone-line
    :init (setq ns-use-srgb-colorspace nil)
    :hook (after-init . telephone-line-mode)))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

;; Color theme
(pcase centaur-theme
  ('default
    (use-package monokai-theme
      :init (load-theme 'monokai t)))
  ('dark
   (use-package spacemacs-theme
     :init (load-theme 'spacemacs-dark t)))
  ('light
   (use-package spacemacs-theme
     :init (load-theme 'spacemacs-light t)))
  ('daylight
   (use-package leuven-theme
     :init (load-theme 'leuven t)))
  (theme
   (if (is-doom-theme-p theme)
       (use-package doom-themes
         :init
         (let ((theme (if (eq centaur-theme 'doom)
                          'doom-one
                        centaur-theme)))
           (load-theme theme t))
         :config
         ;; Enable flashing mode-line on errors
         (doom-themes-visual-bell-config)

         ;; Corrects (and improves) org-mode's native fontification.
         (doom-themes-org-config)

         ;; Enable custom treemacs theme (all-the-icons must be installed!)
         (doom-themes-treemacs-config)

         ;; Enable custom neotree theme (all-the-icons must be installed!)
         (doom-themes-neotree-config)

         ;; Make certain buffers grossly incandescent
         (use-package solaire-mode
           :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
                  (minibuffer-setup . solaire-mode-in-minibuffer)
                  (after-load-theme . solaire-mode-swap-bg))))
     (ignore-errors (load-theme theme t)))))

;; Fonts
(when (display-graphic-p)
  ;; cnfonts doesn't support terminal
  (use-package cnfonts
    :preface
    ;; Fallback to `all-the-icons'.
    (defun cnfonts--set-all-the-icons-fonts (&optional _)
      "Show icons in all-the-icons."
      (when (featurep 'all-the-icons)
        (dolist (charset '(kana han cjk-misc bopomofo gb18030))
          (dolist (font '("all-the-icons" "github-octicons" "FontAwesome" "Material Icons"))
            (set-fontset-font "fontset-default" charset font nil 'append)))))
    :hook ((after-init . cnfonts-enable)
           (cnfonts-set-font-finish . cnfonts--set-all-the-icons-fonts))
    :config
    ;; NOTE: on macOS, the frame size is changed during the startup without below.
    ;; Keep frame size
    (setq cnfonts-keep-frame-size nil)
    (add-hook 'window-setup-hook
              (lambda ()
                (setq cnfonts-keep-frame-size t)))

    ;; Set profiles
    (setq cnfonts-use-cache t)
    (setq cnfonts-profiles
          '("program-normal" "program-large" "program-small" "org-mode" "read-book"))
    (setq cnfonts--profiles-steps '(("program-normal" . 4)
                                    ("program-large" . 5)
                                    ("program-small" . 3)
                                    ("org-mode" . 6)
                                    ("read-book" . 8)))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")

    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :init
      (setq linum-highlight-in-all-buffersp t)
      (custom-set-faces
       `(linum-highlight-face
         ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(size-indication-mode 1)
;; (blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Toggle fullscreen
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
           ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("S-s-<return>" . toggle-frame-fullscreen)
           ("M-S-<return>" . toggle-frame-fullscreen))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
