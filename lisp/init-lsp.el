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

(cond
 ((eq centaur-lsp 'eglot)
  (use-package eglot
    :hook (prog-mode . eglot-ensure)))

 ((eq centaur-lsp 'lsp-mode)
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

    ;; Support LSP in org babel
    ;; https://github.com/emacs-lsp/lsp-mode/issues/377
    (cl-defmacro org-babel-lsp (lang &optional enable-name)
      "Support LANG in org source code block. "
      (cl-check-type lang string)
      (cl-check-type enable-name (or null string))
      (let ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
            (client (intern (format "lsp-%s-enable" (or enable-name lang)))))
        `(progn
           (defun ,edit-pre (babel-info)
             (let ((lsp-file (or (->> babel-info caddr (alist-get :file))
                                 buffer-file-name)))
               (setq-local buffer-file-name lsp-file)
               (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
               (,client)))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))

    ;; FIXME: Project detection
    ;; If nil, use the current directory
    ;; https://github.com/emacs-lsp/lsp-python/issues/28
    (defun my-default-directory ()
      "Returns the current directory."
      default-directory)
    (advice-add #'lsp--suggest-project-root :after-until #'my-default-directory)

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
    :config (org-babel-lsp "go"))

  ;; Python support for lsp-mode using pyls.
  ;; Install: pip install python-language-server
  (use-package lsp-python
    :commands lsp-python-enable
    :hook (python-mode . lsp-python-enable)
    :config (org-babel-lsp "python"))

  ;; Ruby support for lsp-mode using the solargraph gem.
  ;; Install: gem install solargraph
  (use-package lsp-ruby
    :commands lsp-ruby-enable
    :hook (ruby-mode . lsp-ruby-enable)
    :config (org-babel-lsp "ruby"))

  ;; Javascript, Typescript and Flow support for lsp-mode
  ;; Install: npm i -g javascript-typescript-langserver
  (use-package lsp-javascript-typescript
    :commands lsp-javascript-typescript-enable
    :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable)
    :config (org-babel-lsp "js" "javascript-typescript"))

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
           (scss-mode . lsp-scss-enable))
    :config
    (org-babel-lsp "css")
    (org-babel-lsp "sass"))

  ;; HTML support for lsp-mode using vscode-html-languageserver-bin
  ;; Install: npm i -g vscode-html-languageserver-bin
  (use-package lsp-html
    :commands lsp-html-enable
    :hook ((html-mode . lsp-html-enable)
           (web-mode . lsp-html-enable)))

  ;; PHP support for lsp-mode
  ;; Install: composer require felixfbecker/language-server
  ;;          composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
  (use-package lsp-php
    :commands lsp-php-enable
    :hook (php-mode . lsp-php-enable))

  ;; Bash support for lsp-mode using Mads Hartmann's bash-language-server
  ;; Install: npm i -g bash-language-server@1.4.0
  ;; Require Python2.5+, use --python to specify.
  (use-package lsp-sh
    :commands lsp-sh-enable
    :hook (sh-mode . lsp-sh-enable)
    :config
    (if emacs/>=26p
        (org-babel-lsp "shell" "sh")
      (org-babel-lsp "sh")))

  ;; C/C++/Objective-C lang server support for lsp-mode using clang
  ;; Install: brew tap twlz0ne/homebrew-ccls && brew install ccls
  ;;          refer to  https://github.com/MaskRay/ccls/wiki/Getting-started
  (use-package ccls
    :defines projectile-project-root-files-top-down-recurring
    :commands lsp-ccls-enable
    :hook ((c-mode c++-mode objc-mode) . lsp-ccls-enable)
    :config
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
            (append '("compile_commands.json"
                      ".ccls")
                    projectile-project-root-files-top-down-recurring))))

  ;; Rust support for lsp-mode using the Rust Language Server.
  ;; Install: curl https://sh.rustup.rs -sSf | sh
  ;;          rustup component add rls-preview rust-analysis rust-src
  (use-package lsp-rust
    :commands lsp-rust-enable
    :hook (rust-mode . lsp-rust-enable))

  ;; Java support for lsp-mode using the Eclipse JDT Language Server.
  ;; Install:
  ;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
  ;; tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
  (use-package lsp-java
    :commands lsp-java-enable
    :hook (java-mode . lsp-java-enable)
    :config (org-babel-lsp "java"))
  ))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
