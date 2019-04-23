;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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
;; Visual (UI) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Logo
(setq fancy-splash-image centaur-logo)

;; Title
(setq frame-title-format '("Centaur Emacs - %b"))
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
  (and (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil)))

;; Theme
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun standardize-theme (theme)
  "Standardize THEME."
  (pcase theme
    ('default 'doom-one)
    ('classic 'doom-molokai)
    ('doom 'doom-one)
    ('dark 'doom-Iosvkem)
    ('light 'doom-one-light)
    ('daylight 'doom-tomorrow-day)
    (_ theme)))

(defun is-doom-theme-p (theme)
  "Check whether the THEME is a doom theme. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name (standardize-theme theme))))

(defun centaur-load-theme (theme)
  "Set color THEME."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             '(default classic dark light daylight)))))
  (let ((theme (standardize-theme theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(if (is-doom-theme-p centaur-theme)
    (progn
      (use-package doom-themes
        :init (centaur-load-theme centaur-theme)
        :config
        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config)
        ;; Enable custom treemacs theme (all-the-icons must be installed!)
        (doom-themes-treemacs-config)

        ;; Improve treemacs icons
        (with-eval-after-load 'treemacs
          (with-eval-after-load 'all-the-icons
            (when doom-treemacs-use-generic-icons
              (let ((all-the-icons-default-adjust 0)
                    (tab-width 1))
                (setq treemacs-icon-open-png
                      (concat
                       (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
                       "\t"
                       (all-the-icons-octicon "file-directory" :v-adjust 0)
                       "\t")
                      treemacs-icon-closed-png
                      (concat
                       (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust 0.1 :face 'font-lock-doc-face)
                       "\t"
                       (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
                       "\t"))

                ;; File type icons
                (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
                      treemacs-icon-fallback (concat
                                              "\t\t"
                                              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
                                              "\t")
                      treemacs-icon-text treemacs-icon-fallback)

                (dolist (item all-the-icons-icon-alist)
                  (let* ((extension (car item))
                         (func (cadr item))
                         (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
                         (icon (apply func args))
                         (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
                         (value (concat "\t\t" icon "\t")))
                    (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
                      (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
                    (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
                      (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value)))))))))

      ;; Make certain buffers grossly incandescent
      (use-package solaire-mode
        :functions persp-load-state-from-file
        :hook (((after-change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
               (minibuffer-setup . solaire-mode-in-minibuffer)
               (after-load-theme . solaire-mode-swap-bg))
        :config
        (solaire-mode-swap-bg)
        (advice-add #'persp-load-state-from-file
                    :after #'solaire-mode-restore-persp-mode-buffers)))
  (progn
    (ignore-errors
      (centaur-load-theme centaur-theme))))

;; Mode-line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-github t))

(defun mode-line-height ()
  "Get current height of mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :custom-face
  ;; Reset colors since they are too dark in `doom-themes'
  (all-the-icons-silver ((((background dark)) :foreground "#716E68")
                         (((background light)) :foreground "#716E68")))
  (all-the-icons-lsilver ((((background dark)) :foreground "#B9B6AA")
                          (((background light)) :foreground "#7F7869")))
  (all-the-icons-dsilver ((((background dark)) :foreground "#838484")
                          (((background light)) :foreground "#838484")))
  :init
  (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t))
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode  all-the-icons-octicon "markdown" :face all-the-icons-blue)))

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
      :custom-face (linum-highlight-face
                    ((t `(
                          :inherit default
                          :background nil
                          :foreground nil
                          ))))
      :init
      (setq linum-highlight-in-all-buffersp t))))

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

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
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

;; Fullscreen
;; WORKAROUND: To address blank screen issue with child-frame in fullscreen
(when sys/mac-x-p
  (setq ns-use-native-fullscreen nil))
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
           ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("S-s-<return>" . toggle-frame-fullscreen)
           ("M-S-<return>" . toggle-frame-fullscreen))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
