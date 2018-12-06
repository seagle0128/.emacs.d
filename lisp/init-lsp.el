;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
  (require 'init-const)
  (require 'init-custom))

(pcase centaur-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; Install: https://github.com/emacs-lsp/lsp-mode/blob/master/README-NEXT.md#supported-languages
   (use-package lsp
     :ensure lsp-mode
     :diminish lsp-mode
     :hook (((go-mode
              python-mode ruby-mode php-mode
              html-mode web-mode json-mode
              css-mode less-mode sass-mode scss-mode
              js-mode js2-mode typescript-mode
              rust-mode groovy-mode) . lsp)
            (lsp-after-open . lsp-enable-imenu))
     :init
     ;; Support LSP in org babel
     ;; https://github.com/emacs-lsp/lsp-mode/issues/377
     (cl-defmacro lsp-org-babel-enbale (lang)
       "Support LANG in org source code block."
       ;; (cl-check-type lang symbolp)
       (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
              (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
         `(progn
            (defun ,intern-pre (info)
              (let ((lsp-file (or (->> info caddr (alist-get :file))
                                  buffer-file-name)))
                (setq-local buffer-file-name lsp-file)
                (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
                (lsp)))
            (if (fboundp ',edit-pre)
                (advice-add ',edit-pre :after ',intern-pre)
              (progn
                (defun ,edit-pre (info)
                  (,intern-pre info))
                (put ',edit-pre 'function-documentation
                     (format "Prepare local buffer environment for org source block (%s)."
                             (upcase ,lang))))))))

     (defvar org-babel-lang-list '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
     (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
     (dolist (lang org-babel-lang-list)
       (eval `(lsp-org-babel-enbale ,lang)))
     :config (require 'lsp-clients))

   (use-package lsp-ui
     :bind (:map lsp-ui-mode-map
                 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                 ([remap xref-find-references] . lsp-ui-peek-find-references)
                 ("C-c u" . lsp-ui-imenu))
     :hook (lsp-mode . lsp-ui-mode))

   (use-package company-lsp
     :after company
     :defines company-backends
     :functions company-backend-with-yas
     :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

   ;; C/C++/Objective-C support
   ;; Install: brew tap twlz0ne/homebrew-ccls && brew install ccls
   ;;          refer to  https://github.com/MaskRay/ccls/wiki/Getting-started
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
   ;; Install: wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
   ;;          tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
   (use-package lsp-java
     :disabled
     :commands lsp-java-enable
     :hook (java-mode . lsp-java-enable))
   ))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
