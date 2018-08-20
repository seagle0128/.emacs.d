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
;; Install: go get -u github.com/sourcegraph/go-langserver
(use-package lsp-go
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable)
  :config (setq lsp-go-gocode-completion-enabled t))

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
(use-package lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))

;; Ruby support for lsp-mode using the solargraph gem.
;; Install: gem install solargraph
(use-package lsp-ruby
  :ensure nil
  :after lsp-mode
  :commands lsp-ruby-enable
  :hook (ruby-mode . lsp-ruby-enable)
  :init
  (defconst lsp-ruby--get-root
    (lsp-make-traverser
     #'(lambda (dir)
         (directory-files dir nil "\\(Rakefile\\|Gemfile\\)"))))

  (defun lsp-ruby--render-string (str)
    "Render STR with `ruby-mode' syntax highlighting."
    (ignore-errors
      (with-temp-buffer
        (ruby-mode)
        (insert str)
        (font-lock-ensure)
        (buffer-string))))

  (defun lsp-ruby--initialize-client (client)
    "Initial setup for ruby LSP CLIENT."
    (lsp-provide-marked-string-renderer
     client "ruby" 'lsp-ruby--render-string))

  (lsp-define-tcp-client
   lsp-ruby "ruby"
   lsp-ruby--get-root
   '("solargraph" "socket")
   "127.0.0.1"
   7658
   :initialize 'lsp-ruby--initialize-client)

  (lsp-define-stdio-client
   lsp-ruby-mtsmfm "ruby"
   lsp-ruby--get-root
   '("language_server-ruby" "--experimental-features")
   :initialize 'lsp-ruby--initialize-client))

;; Javascript, Typescript and Flow support for lsp-mode
;; Install: npm i -g javascript-typescript-langserver
(use-package lsp-javascript-typescript
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable))

;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
;; Install: npm i -g vscode-css-languageserver-bin
(use-package lsp-css
  :commands (lsp-css-enable
             lsp-less-enable
             lsp-sass-enable
             lsp-scss-enable)
  :hook ((css-mode . lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable)))

;; HTML support for lsp-mode using vscode-html-languageserver-bin
;; Install: npm i -g vscode-html-languageserver-bin
(use-package lsp-html
  :commands lsp-html-enable
  :hook (html-mode . lsp-html-enable))

;; Bash support for lsp-mode using Mads Hartmann's bash-language-server
;; Install: npm i -g bash-language-server
;; Require Python2.5+, use --python to specify.
(use-package lsp-sh
  :ensure nil
  :after lsp-mode
  :commands lsp-sh-enable
  :hook (sh-mode . lsp-sh-enable)
  :init
  (lsp-define-stdio-client lsp-sh
                           "sh"
                           #'(lambda () default-directory)
                           '("bash-language-server" "start")))

;; C/C++/Objective-C language server support for lsp-mode using clang
;; Install: brew install cquery or see https://github.com/cquery-project/cquery/releases
(use-package cquery
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
;; Install: rustup component add rls-preview rust-analysis rust-src
(use-package lsp-rust
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
