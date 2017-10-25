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
;; Install: go get -u github.com/nsf/gocode
(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)

  ;; Install: go get -u github.com/golang/lint/golint
  (use-package golint)

  (use-package go-eldoc
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  ;; Install: go get golang.org/x/tools/cmd/guru
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
      :init (push '(company-go :with company-yasnippet) company-backends))))

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
