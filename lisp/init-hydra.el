;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

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
;; Nice looking hydras.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package hydra
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra--call-interactively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map))

(use-package pretty-hydra
  :defines (display-line-numbers-mode linum-mode)
  :functions set-package-archives centaur-load-theme
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                      :color amaranth :quit-key "q")
    ("Basic"
     (("n" (if (fboundp 'display-line-numbers-mode)
               (display-line-numbers-mode (if display-line-numbers-mode -1 1))
             (linum-mode (if linum-mode -1 1)))
       "line number" :toggle (if (fboundp 'display-line-numbers-mode)
                                 display-line-numbers-mode
                               linum-mode))
      ("a" aggressive-indent-mode "aggressive indent" :toggle t)
      ("h" hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
      ("L" page-break-lines-mode "page break lines" :toggle t)
      ("M" doom-modeline-mode "modern mode-line" :toggle t))
     "Highlight"
     (("l" global-hl-line-mode "line" :toggle t)
      ("P" show-paren-mode "paren" :toggle t)
      ("s" symbol-overlay-mode "symbol" :toggle t)
      ("r" rainbow-mode "rainbow" :toggle t)
      ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
       "whitespace" :toggle show-trailing-whitespace)
      ("d" rainbow-delimiters-mode "delimiter" :toggle t)
      ("i" highlight-indent-guides-mode "indent" :toggle t)
      ("T" hl-todo-mode "todo" :toggle t))
     "Coding"
     (("f" flycheck-mode "flycheck" :toggle t)
      ("F" flymake-mode "flymake" :toggle t)
      ("o" origami-mode "folding" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
     "Version Control"
     (("v" diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("m" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t))
     "Theme"
     (("t d" (centaur-load-theme 'default) "default"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'default)))
      ("t c" (centaur-load-theme 'classic) "classic"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'classic)))
      ("t r" (centaur-load-theme 'colorful) "colorful"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'colorfult)))
      ("t k" (centaur-load-theme 'dark) "dark"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'dark)))
      ("t l" (centaur-load-theme 'light) "light"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'light)))
      ("t y" (centaur-load-theme 'day) "day"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'day)))
      ("t n" (centaur-load-theme 'night) "night"
       :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'night)))
      ("t o" (let ((ivy-initial-inputs-alist '((counsel-load-theme . "doom-"))))
               (ignore ivy-initial-inputs-alist)
               (counsel-load-theme))
       "others"))
     "Package Archive"
     (("p m" (progn (setq centaur-package-archives 'melpa)
                    (set-package-archives centaur-package-archives))
       "melpa" :toggle (eq centaur-package-archives 'melpa))
      ("p i" (progn (setq centaur-package-archives 'melpa-mirror)
                    (set-package-archives centaur-package-archives))
       "melpa mirror" :toggle (eq centaur-package-archives 'melpa-mirror))
      ("p c" (progn (setq centaur-package-archives 'emacs-china)
                    (set-package-archives centaur-package-archives))
       "emacs china" :toggle (eq centaur-package-archives 'emacs-china))
      ("p n" (progn (setq centaur-package-archives 'netease)
                    (set-package-archives centaur-package-archives))
       "netease" :toggle (eq centaur-package-archives 'netease))
      ("p t" (progn (setq centaur-package-archives 'tencent)
                    (set-package-archives centaur-package-archives))
       "tencent" :toggle (eq centaur-package-archives 'tencent))
      ("p u" (progn (setq centaur-package-archives 'tuna)
                    (set-package-archives centaur-package-archives))
       "tuna" :toggle (eq centaur-package-archives 'tuna))))))

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
