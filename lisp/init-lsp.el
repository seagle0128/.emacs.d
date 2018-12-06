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
     :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp)
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
