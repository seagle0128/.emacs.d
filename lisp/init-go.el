;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

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
;; Golang configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Golang
(use-package go-mode
  :ensure-system-package
  ((gocode . "go get -u github.com/mdempsky/gocode")
   (godef . "go get -u github.com/rogpeppe/godef")
   (goimports . "go get -u golang.org/x/tools/cmd/goimports"))
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  (use-package go-dlv
    :ensure-system-package (dlv . "go get -u github.com/derekparker/delve/cmd/dlv"))

  (use-package go-fill-struct
    :ensure-system-package (fillstruct . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct"))

  (use-package go-impl
    :ensure-system-package (impl . "go get -u github.com/josharian/impl"))

  (use-package go-rename
    :ensure-system-package (gorename . "go get -u golang.org/x/tools/cmd/gorename"))

  (use-package golint
    :ensure-system-package (golint . "go get -u github.com/golang/lint/golint"))
  (use-package govet)

  (use-package go-tag
    :ensure-system-package (gomodifytags . "go get -u github.com/fatih/gomodifytags")
    :bind (:map go-mode-map
                ("C-c t" . go-tag-add)
                ("C-c T" . go-tag-remove))
    :config (setq go-tag-args (list "-transform" "camelcase")))

  (use-package gotest
    :ensure-system-package (gotests . "go get -u github.com/cweill/gotests/...")
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run)))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-g" . go-gen-test-dwim)))

  ;; LSP provides the functionalities.
  ;; NOTE: `go-langserver' doesn't support Windows so far.
  (unless centaur-lsp
    ;; `goimports' or `gofmt'
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)

    ;; Go add-ons for Projectile
    ;; Run: M-x `go-projectile-install-tools'
    (with-eval-after-load 'projectile
      (use-package go-projectile
        :commands (go-projectile-mode go-projectile-switch-project)
        :hook ((go-mode . go-projectile-mode)
               (projectile-after-switch-project . go-projectile-switch-project))))

    (use-package go-eldoc
      :hook (go-mode . go-eldoc-setup))

    (use-package go-guru
      :ensure-system-package (guru . "go get -u golang.org/x/tools/cmd/guru")
      :bind (:map go-mode-map
                  ;; ([remap xref-find-definitions] . go-guru-definition)
                  ([remap xref-find-references] . go-guru-referrers)))

    (with-eval-after-load 'company
      (use-package company-go
        :defines company-backends
        :functions company-backend-with-yas
        :init (cl-pushnew (company-backend-with-yas 'company-go) company-backends)))))

;; Local Golang playground for short snippes
(use-package go-playground
  :diminish go-playground-mode
  :commands go-playground-mode)

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
