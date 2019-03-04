;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
;; Language Server Protocol configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(pcase centaur-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish lsp-mode
     :hook (prog-mode . lsp)
     :init
     (setq lsp-auto-guess-root t)       ; Detect project root
     (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
     :config
     ;; Configure LSP clients
     (use-package lsp-clients
       :ensure nil
       :init
       (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))))

   (use-package lsp-ui
     :custom-face
     (lsp-ui-doc-background ((t `(:background nil))))
     :bind (:map lsp-ui-mode-map
                 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                 ([remap xref-find-references] . lsp-ui-peek-find-references)
                 ("C-c u" . lsp-ui-imenu))
     :init
     (setq lsp-ui-doc-enable t
           lsp-ui-doc-include-signature t
           lsp-ui-doc-position 'at-point
           lsp-ui-doc-use-webkit t
           lsp-ui-doc-border (face-foreground 'default)

           lsp-ui-sideline-enable nil
           lsp-ui-sideline-ignore-duplicate t)
     :config
     ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
     ;; https://github.com/emacs-lsp/lsp-ui/issues/243
     (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
       (setq mode-line-format nil)))

   (use-package company-lsp)

   ;; C/C++/Objective-C support
   (use-package ccls
     :defines projectile-project-root-files-top-down-recurring
     :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                      (require 'ccls)
                                                      (lsp)))
     :config
     (with-eval-after-load 'projectile
       (setq projectile-project-root-files-top-down-recurring
             (append '("compile_commands.json"
                       ".ccls")
                     projectile-project-root-files-top-down-recurring))))

   ;; Java support
   (use-package lsp-java
     :hook (java-mode . (lambda ()
                          (require 'lsp-java)
                          (lsp))))
   ))

(unless centaur-lsp
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((lsp-file (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (pcase 'centaur-lsp
               ('eglot (eglot))
               ('lsp-mode (lsp)))))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
