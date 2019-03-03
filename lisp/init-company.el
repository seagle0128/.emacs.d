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
      :functions (all-the-icons-faicon
                  all-the-icons-material
                  all-the-icons-octicon
                  all-the-icons-alltheicon)
      :hook (company-mode . company-box-mode)
      :init (setq company-box-enable-icon (display-graphic-p))
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

        (setq company-box-icons-unknown
              (my-company-box-icon 'octicon "file-text"))

        (setq company-box-icons-elisp
              (list
               (my-company-box-icon 'faicon "cube")        ; Function
               (my-company-box-icon 'faicon "tag")         ; Variable
               (my-company-box-icon 'faicon "cog")         ; Feature
               (my-company-box-icon 'material "palette")   ; Face
               ))

        (setq company-box-icons-yasnippet
              (my-company-box-icon 'octicon "file-code"))  ; Snippet

        (setq company-box-icons-lsp
              `(( 1  . ,(my-company-box-icon 'faicon "file-text-o"))     ; Text
                ( 2  . ,(my-company-box-icon 'faicon "cube"))            ; Method
                ( 3  . ,(my-company-box-icon 'faicon "cube"))            ; Function
                ( 4  . ,(my-company-box-icon 'faicon "cube"))            ; Constructor
                ( 5  . ,(my-company-box-icon 'faicon "tag"))             ; Field
                ( 6  . ,(my-company-box-icon 'faicon "tag"))             ; Variable
                ( 7  . ,(my-company-box-icon 'faicon "cog"))             ; Class
                ( 8  . ,(my-company-box-icon 'faicon "cogs"))            ; Interface
                ( 9  . ,(my-company-box-icon 'alltheicon "less"))        ; Module
                (10  . ,(my-company-box-icon 'faicon "wrench"))          ; Property
                (11  . ,(my-company-box-icon 'faicon "tag"))             ; Unit
                (12  . ,(my-company-box-icon 'faicon "tag"))             ; Value
                (13  . ,(my-company-box-icon 'faicon "file-text-o"))     ; Enum
                (14  . ,(my-company-box-icon 'material "format_align_center")) ; Keyword
                (15  . ,(my-company-box-icon 'material "content_paste")) ; Snippet
                (16  . ,(my-company-box-icon 'material "palette"))       ; Color
                (17  . ,(my-company-box-icon 'faicon "file"))            ; File
                (18  . ,(my-company-box-icon 'faicon "tag"))             ; Reference
                (19  . ,(my-company-box-icon 'faicon "folder"))          ; Folder
                (20  . ,(my-company-box-icon 'faicon "tag"))             ; EnumMember
                (21  . ,(my-company-box-icon 'faicon "tag"))             ; Constant
                (22  . ,(my-company-box-icon 'faicon "cog"))             ; Struct
                (23  . ,(my-company-box-icon 'faicon "bolt"))            ; Event
                (24  . ,(my-company-box-icon 'faicon "tag"))             ; Operator
                (25  . ,(my-company-box-icon 'faicon "cog"))             ; TypeParameter
                )))))

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
