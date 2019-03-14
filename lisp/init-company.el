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
      (setq company-box-show-single-candidate t)
      (setq company-box-max-candidates 50)

      (with-eval-after-load 'all-the-icons
        (eval-and-compile
          (defun my-company-box-icon (family icon &rest args)
            "Defines icons using `all-the-icons' for `company-box'."
            (when icon
              (let ((icon (pcase family
                            ('faicon (apply #'all-the-icons-faicon icon :height 0.9 :v-adjust -0.05 args))
                            ('material (apply #'all-the-icons-material icon :height 0.9 :v-adjust -0.15 args)))))
                (unless (symbolp icon)
                  (concat icon
                          (propertize " " 'face 'variable-pitch)))))))

        (defun company-box-icons--elisp (candidate)
          (when (derived-mode-p 'emacs-lisp-mode)
            (let ((sym (intern candidate)))
              (cond ((fboundp sym) 'Function)
                    ((featurep sym) 'Module)
                    ((facep sym) 'Color)
                    ((boundp sym) 'Variable)
                    ((symbolp sym) 'Text)
                    (t . nil)))))

        (setq company-box-icons-all-the-icons
              `((Unknown . ,(my-company-box-icon 'material "find_in_page"))
                (Text . ,(my-company-box-icon 'material "text_fields"))
                (Method . ,(my-company-box-icon 'faicon "cube" :face 'all-the-icons-purple))
                (Function . ,(my-company-box-icon 'faicon "cube" :face 'all-the-icons-purple))
                (Constructor . ,(my-company-box-icon 'faicon "cube" :face 'all-the-icons-purple))
                (Field . ,(my-company-box-icon 'material "straighten" :face 'all-the-icons-blue))
                (Variable . ,(my-company-box-icon 'material "straighten" :face 'all-the-icons-blue))
                (Class . ,(my-company-box-icon 'material "settings_input_component" :face 'all-the-icons-orange))
                (Interface . ,(my-company-box-icon 'material "share" :face 'all-the-icons-blue))
                (Module . ,(my-company-box-icon 'material "view_module" :face 'all-the-icons-blue))
                (Property . ,(my-company-box-icon 'faicon "wrench"))
                (Unit . ,(my-company-box-icon 'material "settings_system_daydream"))
                (Value . ,(my-company-box-icon 'material "format_align_right" :face 'all-the-icons-blue))
                (Enum . ,(my-company-box-icon 'material "storage" :face 'all-the-icons-orange))
                (Keyword . ,(my-company-box-icon 'material "filter_center_focus"))
                (Snippet . ,(my-company-box-icon 'material "format_align_center"))
                (Color . ,(my-company-box-icon 'material "palette"))
                (File . ,(my-company-box-icon 'faicon "file-o"))
                (Reference . ,(my-company-box-icon 'material "collections_bookmark"))
                (Folder . ,(my-company-box-icon 'faicon "folder-open"))
                (EnumMember . ,(my-company-box-icon 'material "format_align_right" :face 'all-the-icons-blueb))
                (Constant . ,(my-company-box-icon 'faicon "square-o"))
                (Struct . ,(my-company-box-icon 'material "settings_input_component" :face 'all-the-icons-orange))
                (Event . ,(my-company-box-icon 'faicon "bolt" :face 'all-the-icons-orange))
                (Operator . ,(my-company-box-icon 'material "control_point"))
                (TypeParameter . ,(my-company-box-icon 'faicon "arrows"))
                (Template . ,(my-company-box-icon 'material "format_align_center")))))))

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
