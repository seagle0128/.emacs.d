;; init-corfu.el --- Initialize corfu configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Vincent Zhang

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
;; Auto-completion configurations.
;;

;;; Code:

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Trigger auto completion with 2 chars
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package kind-icon
  :when (icons-displayable-p)
  :after corfu
  :functions nerd-icons-codicon
  :init
  (defconst corfu-kind-icon-mapping
    `((array . ,(nerd-icons-codicon "nf-cod-symbol_array" :face 'font-lock-type-face))
      (boolean . ,(nerd-icons-codicon "nf-cod-symbol_boolean" :face 'font-lock-builtin-face))
      (class . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'font-lock-type-face))
      (color . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success) )
      (command . ,(nerd-icons-codicon "nf-cod-terminal" :face 'default) )
      (constant . ,(nerd-icons-codicon "nf-cod-symbol_constant" :face 'font-lock-constant-face) )
      (constructor . ,(nerd-icons-codicon "nf-cod-triangle_right" :face 'font-lock-function-name-face) )
      (enummember . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
      (enum-member . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
      (enum . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'font-lock-builtin-face) )
      (event . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'font-lock-warning-face) )
      (field . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-variable-name-face) )
      (file . ,(nerd-icons-codicon "nf-cod-symbol_file" :face 'font-lock-string-face) )
      (folder . ,(nerd-icons-codicon "nf-cod-folder" :face 'font-lock-doc-face) )
      (interface . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'font-lock-type-face) )
      (keyword . ,(nerd-icons-codicon "nf-cod-symbol_keyword" :face 'font-lock-keyword-face) )
      (macro . ,(nerd-icons-codicon "nf-cod-symbol_misc" :face 'font-lock-keyword-face) )
      (magic . ,(nerd-icons-codicon "nf-cod-wand" :face 'font-lock-builtin-face) )
      (method . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
      (function . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
      (module . ,(nerd-icons-codicon "nf-cod-file_submodule" :face 'font-lock-preprocessor-face) )
      (numeric . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'font-lock-builtin-face) )
      (operator . ,(nerd-icons-codicon "nf-cod-symbol_operator" :face 'font-lock-comment-delimiter-face) )
      (param . ,(nerd-icons-codicon "nf-cod-symbol_parameter" :face 'default) )
      (property . ,(nerd-icons-codicon "nf-cod-symbol_property" :face 'font-lock-variable-name-face) )
      (reference . ,(nerd-icons-codicon "nf-cod-references" :face 'font-lock-variable-name-face) )
      (snippet . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face) )
      (string . ,(nerd-icons-codicon "nf-cod-symbol_string" :face 'font-lock-string-face) )
      (struct . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'font-lock-variable-name-face) )
      (text . ,(nerd-icons-codicon "nf-cod-text_size" :face 'font-lock-doc-face) )
      (typeparameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
      (type-parameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
      (unit . ,(nerd-icons-codicon "nf-cod-symbol_ruler" :face 'font-lock-constant-face) )
      (value . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-builtin-face) )
      (variable . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face) )
      (t . ,(nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face))))

  (defsubst nerd-icon--metadata-get (metadata type-name)
    "Get METADATA for keyword TYPE-NAME from the completion properties."
    (or
     (plist-get completion-extra-properties (intern (format ":%s" type-name)))
     (cdr (assq (intern type-name) metadata))))

  (defsubst nerd-icon-formatted (kind)
    "Get icon for KIND."
    (let* ((icon (alist-get kind corfu-kind-icon-mapping))
           (icon-face (get-text-property 0 'face icon))
           (icon-bg (plist-get icon-face :inherit))
           (icon-pad (propertize " " 'face (append '(:height 0.5) icon-bg)))
           (item-pad (propertize " " 'face '(:height 0.5))))
      (concat icon-pad icon icon-pad item-pad)))

  (defun nerd-icon-margin-formatter (metadata)
    "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
    (if-let ((kind-func (nerd-icon--metadata-get metadata "company-kind")))
        (lambda (cand)
          (if-let ((kind (funcall kind-func cand)))
              (nerd-icon-formatted kind)
            (nerd-icon-formatted t)))))

  (add-to-list 'corfu-margin-formatters #'nerd-icon-margin-formatter))

;; Add extensions
(use-package cape
  :init
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  :config
  (require 'cape-yasnippet)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
