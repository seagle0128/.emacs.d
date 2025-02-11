;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Vincent Zhang

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
;; Golang configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Golang
(use-package go-mode
  :functions (go-install-tools exec-path-from-shell-copy-envs)
  :autoload godoc-gogetdoc
  :bind (:map go-mode-map
         ("<f1>" . godoc))
  :init
  (setq godoc-at-point-function #'godoc-gogetdoc)

  ;; Install tools
  (defconst go--tools
    '("golang.org/x/tools/gopls"
      "golang.org/x/tools/cmd/goimports"
      "honnef.co/go/tools/cmd/staticcheck"
      "github.com/go-delve/delve/cmd/dlv"
      "github.com/zmb3/gogetdoc"
      "github.com/josharian/impl"
      "github.com/cweill/gotests/..."
      "github.com/fatih/gomodifytags"
      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  (defun go-install-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-install-tools))

  ;; Misc
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)

  (use-package go-tag
    :bind (:map go-mode-map
           ("C-c t a" . go-tag-add)
           ("C-c t r" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
           ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
           ("C-c t f" . go-test-current-file)
           ("C-c t t" . go-test-current-test)
           ("C-c t j" . go-test-current-project)
           ("C-c t b" . go-test-current-benchmark)
           ("C-c t c" . go-test-current-coverage)
           ("C-c t x" . go-run))))

(when (centaur-treesit-available-p)
  (use-package go-ts-mode
    :init (setq go-ts-mode-indent-offset 4)))

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
