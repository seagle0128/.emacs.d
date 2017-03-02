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
                    (interactive)
                    (message "Revert this buffer.")
                    (revert-buffer t t)))

;; Which key
(use-package which-key
  :defer t
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook 'which-key-mode))

;; Browse url
(use-package browse-url-dwim
  :defer t
  :init (add-hook 'after-init-hook 'browse-url-dwim-mode))

;; Tramp
(use-package tramp
  :defer t
  :init (let ((val (if (executable-find "plink") "plink" "ssh")))
          (setq tramp-default-method val)))

;; Tree explorer
(use-package neotree
  :defer t
  :defines projectile-switch-project-action
  :bind (([f12] . neotree-toggle)
         ([C-f12] . neotree-toggle))
  :config
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char))

  (bind-key "i" 'neotree-enter-horizontal-split neotree-mode-map)
  (bind-key "I" 'neotree-enter-vertical-split neotree-mode-map)

  ;; Integrate with projectile
  (eval-after-load 'projectile
    '(setq projectile-switch-project-action 'neotree-projectile-action))
  )

;; Dash
(use-package dash-at-point
  :defer t
  :if sys/macp
  :bind (("\C-cd" . dash-at-point)
         ("\C-ce" . dash-at-point-with-docset)))

;; Youdao Dictionay
(use-package youdao-dictionary
  :defer t
  :defines popwin:special-display-config
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
(use-package fzf :defer t)
(use-package ack :defer t)

(use-package ag
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :defer t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; Search Chinese by Pinyin
(use-package pinyin-search
  :defer t
  :bind (("C-c C-s" . pinyin-search)))

;; Jump to definition via ag/rg/grep
(use-package dumb-jump
  :defer t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go))
  :config
  (when (featurep 'ivy)
    (add-hook 'ivy-mode-hook '(lambda () (setq dumb-jump-selector 'ivy)))))

;; Junk file
(use-package open-junk-file
  :defer t
  :commands open-junk-file)

;; Side-by-side diff view
(use-package diffview :defer t)

;; Description: Extensions to `info.el'
(use-package info+
  :defer t
  :init
  (add-hook 'Info-mode-hook '(lambda () (require 'info+)))
  (setq Info-fontify-angle-bracketed-flag nil))

;; Misc
(use-package copyit :defer t)
(use-package htmlize :defer t)
(use-package list-environment :defer t)
(use-package memory-usage :defer t)
(use-package restart-emacs :defer t)
(use-package try :defer t)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
