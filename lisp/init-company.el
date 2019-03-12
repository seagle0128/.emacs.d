;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

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
;; Auto-completion configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  ;; Icons and quickhelp
  (when emacs/>=26p
    (use-package company-box
      :diminish
      :hook (company-mode . company-box-mode)
      :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
      :config
      (setq company-box-backends-colors nil)

      (with-eval-after-load 'all-the-icons
        (eval-and-compile
          (defun my-company-box-icon (family icon &rest args)
            "Defines icons using `all-the-icons' for `company-box'."
            (when icon
              (let ((icon (pcase family
                            ('octicon (all-the-icons-octicon icon :v-adjust -0.05 args))
                            ('faicon (all-the-icons-faicon icon :v-adjust -0.0575))
                            ('material (all-the-icons-material icon :v-adjust -0.225 args))
                            ('alltheicon (all-the-icons-alltheicon icon args)))))
                (unless (symbolp icon)
                  (concat icon
                          (propertize " " 'face 'variable-pitch)))))))

        (setq company-box-icons-all-the-icons
              `((Unknown . ,(my-company-box-icon 'octicon "file-text"))
                (Text . ,(my-company-box-icon 'faicon "file-text-o"))
                (Method . ,(my-company-box-icon 'faicon "cube"))
                (Function . ,(my-company-box-icon 'faicon "cube"))
                (Constructor . ,(my-company-box-icon 'faicon "cube"))
                (Field . ,(my-company-box-icon 'faicon "tag"))
                (Variable . ,(my-company-box-icon 'faicon "tag"))
                (Class . ,(my-company-box-icon 'faicon "cog"))
                (Interface . ,(my-company-box-icon 'faicon "cogs"))
                (Module . ,(my-company-box-icon 'alltheicon "less"))
                (Property . ,(my-company-box-icon 'faicon "wrench"))
                (Unit . ,(my-company-box-icon 'faicon "tag"))
                (Value . ,(my-company-box-icon 'faicon "tag"))
                (Enum . ,(my-company-box-icon 'faicon "file-text-o"))
                (Keyword . ,(my-company-box-icon 'material "format_align_center"))
                (Snippet . ,(my-company-box-icon 'material "content_paste"))
                (Color . ,(my-company-box-icon 'material "palette"))
                (File . ,(my-company-box-icon 'faicon "file"))
                (Reference . ,(my-company-box-icon 'faicon "tag"))
                (Folder . ,(my-company-box-icon 'faicon "folder"))
                (EnumMember . ,(my-company-box-icon 'faicon "tag"))
                (Constant . ,(my-company-box-icon 'faicon "tag"))
                (Struct . ,(my-company-box-icon 'faicon "cog"))
                (Event . ,(my-company-box-icon 'faicon "bolt"))
                (Operator . ,(my-company-box-icon 'faicon "tag"))
                (TypeParameter . ,(my-company-box-icon 'faicon "cog"))
                (Template . ,(my-company-box-icon 'octicon "file-code")))))))

  ;; Popup documentation for completion candidates
  (when (and (not emacs/>=26p) (display-graphic-p))
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 0.8))))

(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
