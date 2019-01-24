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
         ;; ("C-c C-y" . company-yasnippet)
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
        (setq company-box-icons-unknown
              (all-the-icons-octicon "file-text" :v-adjust -0.05))

        (setq company-box-icons-elisp
              (list
               (all-the-icons-faicon "cube" :v-adjust -0.0575 :face 'font-lock-constant-face)       ; Function
               (all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face)         ; Variable
               (all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face)         ; Feature
               (all-the-icons-material "palette" :v-adjust -0.2)      ; Face
               ))

        (setq company-box-icons-yasnippet
              (all-the-icons-octicon "file-code" :v-adjust -0.05))    ; Snippet

        (setq company-box-icons-lsp
              `(( 1  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575))     ; Text
                ( 2  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face font-lock-constant-face))            ; Method
                ( 3  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face font-lock-constant-face))            ; Function
                ( 4  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face font-lock-constant-face))            ; Constructor
                ( 5  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Field
                ( 6  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Variable
                ( 7  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Class
                ( 8  . ,(all-the-icons-faicon "cogs" :v-adjust -0.0575))            ; Interface
                ( 9  . ,(all-the-icons-alltheicon "less"))                          ; Module
                (10  . ,(all-the-icons-faicon "wrench" :v-adjust -0.0575))          ; Property
                (11  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Unit
                (12  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face))             ; Value
                (13  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575 :face 'font-lock-warning-face))     ; Enum
                (14  . ,(all-the-icons-material "format_align_center" :v-adjust -0.2))             ; Keyword
                (15  . ,(all-the-icons-material "content_paste" :v-adjust -0.2))    ; Snippet
                (16  . ,(all-the-icons-material "palette" :v-adjust -0.2))          ; Color
                (17  . ,(all-the-icons-faicon "file" :v-adjust -0.0575))            ; File
                (18  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Reference
                (19  . ,(all-the-icons-faicon "folder" :v-adjust -0.0575))          ; Folder
                (20  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face))             ; EnumMember
                (21  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face))             ; Constant
                (22  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Struct
                (23  . ,(all-the-icons-faicon "bolt" :v-adjust -0.0575 :face 'font-lock-warning-face))            ; Event
                (24  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Operator
                (25  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; TypeParameter
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
