;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Configurations for Golang.
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

;; Golang
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u github.com/alecthomas/gometalinter
;; FIXME: `go-guru' doesn't work on Windows. Use `godef' instead.
;; https://github.com/dominikh/go-mode.el/issues/218
(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("<f1>" . godoc-at-point))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package golint)

  (use-package go-eldoc
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  (use-package go-guru
    :bind (:map go-mode-map
                ("M-." . go-guru-definition)
                ("M-?" . go-guru-referrers)))

  (with-eval-after-load 'projectile
    (use-package go-projectile
      :commands (go-projectile-mode go-projectile-switch-project)
      :init
      (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
      (add-hook 'go-mode-hook #'go-projectile-mode)))

  (with-eval-after-load 'company
    (use-package company-go
      :init (push '(company-go :with company-yasnippet) company-backends)))

  (with-eval-after-load 'flycheck
    ;; gometalinter --install --update
    (use-package flycheck-gometalinter
      :init (add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup)
      :config (setq flycheck-gometalinter-fast t))))

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
