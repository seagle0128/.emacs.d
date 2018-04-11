;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.3.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Configurations for lsp.
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

(when (>= emacs-major-version 25)
  ;; Emacs client for the Language Server Protocol
  ;; https://github.com/emacs-lsp/lsp-mode
  (use-package lsp-mode
    :diminish lsp-mode
    :config
    (use-package lsp-ui
      :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
      :bind (:map lsp-ui-mode-map
                  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                  ([remap xref-find-references] . lsp-ui-peek-find-references))
      :init (add-hook 'lsp-mode-hook 'lsp-ui-mode))

    (with-eval-after-load 'company
      (use-package company-lsp
        :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))))

  ;; Go support for lsp-mode using Sourcegraph's Go Language Server
  ;; Install: go get github.com/sourcegraph/go-langserver
  (use-package lsp-go
    :commands lsp-go-enable
    :init (add-hook 'go-mode-hook #'lsp-go-enable))

  ;; Python support for lsp-mode using pyls.
  ;; Install: pip install python-language-server
  (use-package lsp-python
    :commands lsp-python-enable
    :init (add-hook 'python-mode-hook #'lsp-python-enable))

  ;; Javascript, Typescript and Flow support for lsp-mode
  ;; Install: npm i -g javascript-typescript-langserver
  (use-package lsp-javascript-typescript
    :commands lsp-javascript-typescript-enable
    :init
    (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
    (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable))

  ;; Java support for lsp-mode using the Eclipse JDT Language Server.
  ;; Install:
  ;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
  ;; tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
  (use-package lsp-java
    :commands lsp-java-enable
    :init (add-hook 'java-mode-hook #'lsp-java-enable)))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
