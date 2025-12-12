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

;; Install tools
(defvar go-tools
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
  (dolist (pkg go-tools)
    (set-process-sentinel
     (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
     (lambda (proc _)
       (let ((status (process-exit-status proc)))
         (if (= 0 status)
             (message "Installed %s" pkg)
           (message "Failed to install %s: %d" pkg status)))))))

;; Configure Golang automatically
(defvar go-keymap (if (centaur-treesit-available-p)
                      'go-ts-mode-map
                    'go-mode-map)
  "The keymap for Golang.")

(defun go-auto-config ()
  "Configure Golang automatically."
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Try to install go tools if `gopls' is not found
  (when (and (executable-find "go")
             (not (executable-find "gopls")))
    (go-install-tools))

  ;; Misc. tools
  (use-package go-fill-struct)

  (use-package go-gen-test
    :bind (:map go-keymap
           ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-keymap
           ("C-c t f" . go-test-current-file)
           ("C-c t t" . go-test-current-test)
           ("C-c t j" . go-test-current-project)
           ("C-c t b" . go-test-current-benchmark)
           ("C-c t c" . go-test-current-coverage)
           ("C-c t x" . go-run))))

;; Golang
(if (centaur-treesit-available-p)
    (use-package go-ts-mode
      :functions (centaur-treesit-available-p exec-path-from-shell-copy-envs)
      :mode (("\\.go\\'" . go-ts-mode)
             ("/go\\.mod\\'" . go-mod-ts-mode))
      :custom (go-ts-mode-indent-offset 4)
      :config (go-auto-config))
  (use-package go-mode
    :defines go-mode-map
    :autoload godoc-gogetdoc
    :bind (:map go-mode-map
           ("<f1>" . godoc))
    :custom (godoc-at-point-function #'godoc-gogetdoc)
    :config
    (go-auto-config)

    ;; Misc.
    (use-package go-dlv)
    (use-package go-impl)

    (use-package go-tag
      :bind (:map go-mode-map
             ("C-c t a" . go-tag-add)
             ("C-c t r" . go-tag-remove))
      :custom (go-tag-args (list "-transform" "camelcase")))))

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
