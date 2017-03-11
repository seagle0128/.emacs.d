;; init-utils.el --- Initialize basic configurations.
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
;;             Utils configurations.
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

;; Dos2Unix
(defun dos2unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;; Revert buffer
(bind-key "<f5>" '(lambda ()
                    "Revert the current buffer."
                    (interactive)
                    (message "Revert this buffer.")
                    (revert-buffer t t)))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook 'which-key-mode))

;; Context-sensitive external browse URL or Internet search
(use-package browse-url-dwim
  :init (add-hook 'after-init-hook 'browse-url-dwim-mode))

;; Show imenu entries in a seperate buffer
(use-package imenu-list
  :bind ([f9] . imenu-list-smart-toggle))

;; Tree explorer
(use-package neotree
  :defines projectile-switch-project-action
  :bind (([f8] . neotree-toggle)
         :map neotree-mode-map
         ("i" . neotree-enter-horizontal-split)
         ("I" . neotree-enter-vertical-split))
  :config
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char))

  ;; Integrate with projectile
  (eval-after-load 'projectile
    '(setq projectile-switch-project-action 'neotree-projectile-action)))

;; Dash
;; only avaliable on macOS
(when sys/macp
  (use-package dash-at-point
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset)))
  )

;; Youdao Dictionay
(use-package youdao-dictionary
  :commands youdao-dictionary--region-or-word youdao-dictionary--format-result
  :bind (("C-c Y" . youdao-dictionary-search-at-point)
         ("C-c y" . youdao-dictionary-search-at-point+))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  (eval-after-load 'popwin
    '(push "*Youdao Dictionary*" popwin:special-display-config))

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  ;; Use pos-tip instead of popup to display results
  (if (display-graphic-p)
      (eval-after-load 'pos-tip
        '(defun youdao-dictionary-search-at-point+ ()
           "Search word at point and display results with pos-tip."
           (interactive)
           (let ((word (youdao-dictionary--region-or-word))
                 (x-gtk-use-system-tooltips t))
             (if word
                 (pos-tip-show (youdao-dictionary--format-result word)
                               nil nil nil 0)
               (message "Nothing to look up"))))))
  )

;; Search
(use-package fzf)
(use-package ack)

(use-package ag
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; Jump to definition via ag/rg/grep
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go))
  :config
  (when (featurep 'ivy)
    (add-hook 'ivy-mode-hook
              '(lambda () (setq dumb-jump-selector 'ivy)))))

;; Side-by-side diff view
(use-package diffview)

;; Extensions to `Dired'
(use-package dired+
  :after dired
  :init (setq diredp-hide-details-initially-flag nil)
  :config (diredp-toggle-find-file-reuse-dir 1))

;; Provide menu/dialogue for dired sort options
(use-package dired-sort-menu+ :after dired)

;; Extensions to `Info'
(use-package info+
  :after info
  :init (setq Info-fontify-angle-bracketed-flag nil))

;; Misc
(use-package copyit)
(use-package htmlize)
(use-package list-environment)
(use-package memory-usage)
(use-package open-junk-file)
(use-package restart-emacs)
(use-package try)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
