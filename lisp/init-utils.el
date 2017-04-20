;; init-utils.el --- Initialize basic configurations.
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

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))
(bind-key "<f5>" 'revert-current-buffer)

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
  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'neotree-projectile-action)))

;; Dash
;; only avaliable on macOS
(when sys/macp
  (use-package dash-at-point
    :defer-install t
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

;; Youdao Dictionay
(use-package youdao-dictionary
  :defer-install t
  :commands (youdao-dictionary--region-or-word
             youdao-dictionary--format-result)
  :bind (("C-c Y" . youdao-dictionary-search-at-point)
         ("C-c y" . youdao-dictionary-search-at-point+))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  (with-eval-after-load 'popwin
    (push "*Youdao Dictionary*" popwin:special-display-config))

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  ;; Use pos-tip instead of popup to display results
  (if (display-graphic-p)
      (with-eval-after-load 'pos-tip
        (defun youdao-dictionary-search-at-point+ ()
          "Search word at point and display results with pos-tip."
          (interactive)
          (let ((word (youdao-dictionary--region-or-word))
                (x-gtk-use-system-tooltips t))
            (if word
                (pos-tip-show (youdao-dictionary--format-result word)
                              nil nil nil 0)
              (message "Nothing to look up")))))))

;; Search
(use-package fzf
  :defer-install t
  :commands (fzf fzf-directory))

(use-package ack
  :defer-install t
  :commands ack)

(use-package ag
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package ripgrep)

;; Jump to definition via ag/rg/grep
(use-package dumb-jump
  :defer-install t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go))
  :config
  (when (featurep 'ivy)
    (add-hook 'ivy-mode-hook
              '(lambda () (setq dumb-jump-selector 'ivy)))))

;; Side-by-side diff view
(use-package diffview
  :defer-install t
  :commands (diffview-region diffview-current diffview-message))

;; Text mode directory tree. Similar with beyond compare
(use-package ztree
  :defer-install t
  :commands (ztree-diff ztree-dir))

;; Extensions to `Dired'
(use-package dired+
  :after dired
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
  :config (diredp-toggle-find-file-reuse-dir 1))

;; Provide menu/dialogue for dired sort options
(use-package dired-sort-menu+ :after dired)

;; Extensions to `Info'
(use-package info+
  :after info
  :init (setq Info-fontify-angle-bracketed-flag nil))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :defer-install t
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; Emacs StartUp Profiler
(use-package esup
  :defer-install t
  :commands esup)

;; Misc
(use-package copyit)
(use-package htmlize)

(use-package list-environment
  :defer-install t
  :commands list-environment)

(use-package memory-usage
  :defer-install t
  :commands memory-usage)

(use-package open-junk-file
  :defer-install t
  :commands open-junk-file)

(use-package restart-emacs
  :defer-install t
  :commands restart-emacs)

(use-package try
  :defer-install t
  :commands (try try-and-refresh))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
