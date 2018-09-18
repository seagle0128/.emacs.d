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

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :diminish lsp-mode
  :config
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (setq create-lockfiles nil)

  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0))

(use-package company-lsp
  :after company
  :defines company-backends
  :functions company-backend-with-yas
  :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

;; Go support for lsp-mode using Sourcegraph's Go Language Server
(use-package lsp-go
  :ensure-system-package
  (go-langserver . "go get -u github.com/sourcegraph/go-langserver")
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable))

;; Python support for lsp-mode using pyls.
(use-package lsp-python
  :ensure-system-package (pyls . "pip install -U python-language-server")
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))

;; Ruby support for lsp-mode using the solargraph gem.
(use-package lsp-ruby
  :ensure-system-package (solargraph . "sudo gem install solargraph")
  :commands lsp-ruby-enable
  :hook (ruby-mode . lsp-ruby-enable))

;; Javascript, Typescript and Flow support for lsp-mode
(use-package lsp-javascript-typescript
  :ensure-system-package
  (javascript-typescript-langserver . "sudo npm i -g javascript-typescript-langserver")
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable))

;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
(use-package lsp-css
  :ensure-system-package
  (vscode-css-languageserver-bin . "sudo npm i -g vscode-css-languageserver-bin")
  :commands (lsp-css-enable
             lsp-less-enable
             lsp-sass-enable
             lsp-scss-enable)
  :hook ((css-mode . lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable)))

;; HTML support for lsp-mode using vscode-html-languageserver-bin
(use-package lsp-html
  :ensure-system-package
  (vscode-html-languageserver-bin . "sudo npm i -g vscode-html-languageserver-bin")
  :commands lsp-html-enable
  :hook (html-mode . lsp-html-enable))

;; PHP support for lsp-mode
;; composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
(use-package lsp-php
  :ensure-system-package
  (php-language-server . "composer require felixfbecker/language-server")
  :commands lsp-php-enable
  :hook (php-mode . lsp-php-enable))

;; Bash support for lsp-mode using Mads Hartmann's bash-language-server
;; Require Python2.5+, use --python to specify.
(use-package lsp-sh
  :ensure nil
  :after lsp-mode
  :ensure-system-package
  (bash-language-server . "sudo npm i -g bash-language-server@1.4.0")
  :commands lsp-sh-enable
  :hook (sh-mode . lsp-sh-enable)
  :init
  (lsp-define-stdio-client lsp-sh
                           "sh"
                           #'(lambda () default-directory)
                           '("bash-language-server" "start")))

;; C/C++/Objective-C language server support for lsp-mode using clang
;; Install: brew install cquery or download binary from https://github.com/cquery-project/cquery/releases.
(use-package cquery
  :ensure-system-package cquery
  :defines projectile-project-root-files-top-down-recurring
  :commands lsp-cquery-enable
  :hook ((c-mode c++-mode objc-mode) . lsp-cquery-enable)
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".cquery")
                  projectile-project-root-files-top-down-recurring))))

;; Rust support for lsp-mode using the Rust Language Server.
(use-package lsp-rust
  :ensure-system-package
  ((rustup . "curl https://sh.rustup.rs -sSf | sh")
   (rls . "rustup component add rls-preview rust-analysis rust-src"))
  :commands lsp-rust-enable
  :hook (rust-mode . lsp-rust-enable))

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
(use-package lsp-java
  :commands lsp-java-enable
  :hook (java-mode . lsp-java-enable))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
