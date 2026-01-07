;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Vincent Zhang

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
;; Nice looking hydras.
;;

;;; Code:

(use-package hydra
  :defines (consult-imenu-config posframe-border-width)
  :functions childframe-completion-workable-p
  :hook (after-load-theme . hydra-set-posframe-appearance)
  :init
  (with-eval-after-load 'consult-imenu
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                             :types ((?f "Functions" font-lock-function-name-face)
                                     (?h "Hydras"    font-lock-constant-face)
                                     (?m "Macros"    font-lock-function-name-face)
                                     (?p "Packages"  font-lock-constant-face)
                                     (?t "Types"     font-lock-type-face)
                                     (?v "Variables" font-lock-variable-name-face))))))

  (defun hydra-set-posframe-appearance ()
    "Set appearance of hydra."
    (when (childframe-completion-workable-p)
      (setq hydra-hint-display-type 'posframe)
      (setq hydra-posframe-show-params
            `(:left-fringe 8
              :right-fringe 8
              :internal-border-width ,posframe-border-width
              :internal-border-color ,(face-background 'posframe-border nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              :poshandler posframe-poshandler-frame-center-near-bottom))))
  (hydra-set-posframe-appearance))

(use-package pretty-hydra
  :functions icons-displayable-p
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face 'mode-line-emphasis))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))
  :config
  (with-no-warnings
    ;; Define hydra for global toggles
    (pretty-hydra-define toggles-hydra
      (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on")
       :color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("n" display-line-numbers-mode "line number" :toggle t)
        ("a" global-aggressive-indent-mode "aggressive indent *" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete *" :toggle t)
        ("e" electric-pair-mode "electric pair *" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines *" :toggle t)
        ("b" display-battery-mode "battery *" :toggle t)
        ("i" display-time-mode "time *" :toggle t)
        ("m" doom-modeline-mode "modern mode-line *" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line *" :toggle t)
        ("h p" show-paren-mode "parenthesis *" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" global-colorful-mode "color *" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" indent-bars-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo *" :toggle t))
       "Program"
       (("f" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "current function *" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter *" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter *" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter *" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Theme"
       (("t a" (centaur-load-theme 'auto) "auto"
         :toggle (eq centaur-theme 'auto) :exit t)
        ("t r" (centaur-load-theme 'random) "random"
         :toggle (eq centaur-theme 'random) :exit t)
        ("t s" (centaur-load-theme 'system) "system"
         :toggle (eq centaur-theme 'system) :exit t)
        ("t d" (centaur-load-theme 'default) "default"
         :toggle (centaur-theme-enable-p 'default) :exit t)
        ("t p" (centaur-load-theme 'pro) "pro"
         :toggle (centaur-theme-enable-p 'pro) :exit t)
        ("t k" (centaur-load-theme 'dark) "dark"
         :toggle (centaur-theme-enable-p 'dark) :exit t)
        ("t l" (centaur-load-theme 'light) "light"
         :toggle (centaur-theme-enable-p 'light) :exit t)
        ("t w" (centaur-load-theme 'warm) "warm"
         :toggle (centaur-theme-enable-p 'warm) :exit t)
        ("t c" (centaur-load-theme 'cold) "cold"
         :toggle (centaur-theme-enable-p 'cold) :exit t)
        ("t y" (centaur-load-theme 'day) "day"
         :toggle (centaur-theme-enable-p 'day) :exit t)
        ("t n" (centaur-load-theme 'night) "night"
         :toggle (centaur-theme-enable-p 'night) :exit t)
        ("t o" (centaur-load-theme
                (intern (completing-read "Load custom theme: "
                                         (mapcar #'symbol-name
				                                 (custom-available-themes)))))
         "others"
         :toggle (not (or (rassoc (car custom-enabled-themes) centaur-theme-alist)
                          (rassoc (cadr custom-enabled-themes) centaur-theme-alist)))
         :exit t))
       "Package Archive"
       (("p m" (centaur-set-package-archives 'melpa t)
         "melpa" :toggle (eq centaur-package-archives 'melpa) :exit t)
        ("p b" (centaur-set-package-archives 'bfsu t)
         "bfsu" :toggle (eq centaur-package-archives 'bfsu) :exit t)
        ("p i" (centaur-set-package-archives 'iscas t)
         "iscas" :toggle (eq centaur-package-archives 'iscas) :exit t)
        ("p n" (centaur-set-package-archives 'netease t)
         "netease" :toggle (eq centaur-package-archives 'netease) :exit t)
        ("p s" (centaur-set-package-archives 'sjtu t)
         "sjtu" :toggle (eq centaur-package-archives 'sjtu) :exit t)
        ("p t" (centaur-set-package-archives 'tuna t)
         "tuna" :toggle (eq centaur-package-archives 'tuna) :exit t)
        ("p u" (centaur-set-package-archives 'ustc t)
         "ustc" :toggle (eq centaur-package-archives 'ustc) :exit t)
        ("p T" (centaur-test-package-archives) "speed test" :exit t))))))

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
