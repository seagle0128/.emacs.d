;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.2.0
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
;;
;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;;
(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  ;; `goimports' or `gofmt'
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-playground)
  (use-package golint)
  (use-package govet)

  (use-package go-eldoc
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  (use-package go-guru
    :bind (:map go-mode-map
                ("M-." . go-guru-definition)
                ("M-?" . go-guru-referrers))
    :config
    ;; Fix: `go-guru' doesn't work on Windows https://github.com/dominikh/go-mode.el/issues/218
    (defun go-guru--goto-pos (posn other-window)
      "Find the file containing the position POSN (of the form `file:line:col')
set the point to it, switching the current buffer."
      (let ((file-line-pos (split-string posn ":")))
        (if (eq system-type 'windows-nt)
            (let ((first-file-line-pos (car file-line-pos)))
              (if (string-match-p "^[a-z]$" first-file-line-pos);; looks like a drive name ...
                  (setq file-line-pos (cdr file-line-pos));; .. so remove it
                )))
        (funcall (if other-window #'find-file-other-window #'find-file) (car file-line-pos))
        (goto-char (point-min))
        (forward-line (1- (string-to-number (cadr file-line-pos))))
        (go-guru--goto-byte-column (string-to-number (cl-caddr file-line-pos))))))

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c t" . go-tag-add)
                ("C-c T" . go-tag-remove)))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run)))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-g" . go-gen-test-dwim)))

  (with-eval-after-load 'company
    (use-package company-go
      :init (cl-pushnew (company-backend-with-yas 'company-go) company-backends)))

  (with-eval-after-load 'projectile
    ;; M-x `go-projectile-install-tools'
    (use-package go-projectile
      :commands (go-projectile-mode go-projectile-switch-project)
      :init
      (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
      (add-hook 'go-mode-hook #'go-projectile-mode))))

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
