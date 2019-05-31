;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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
;;
;; Go packages:
;; go get -u github.com/mdempsky/gocode # github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/gotype
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;; go get -u github.com/uudashr/gopkgs/cmd/gopkgs

(eval-when-compile
  (require 'init-custom))

;; Golang
(use-package go-mode
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  ;; Format with `goimports' if possible, otherwise using `gofmt'
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-rename)
  (use-package golint)
  (use-package govet)

  (use-package go-impl
    :functions (go-root-and-paths go-packages-fd)
    :config
    ;; `go-packages-native', remiplement it.
    (cond
     ((executable-find "gopkgs")
      (defun go-packages-gopkgs()
        "Return a list of all Go packages, using `gopkgs'."
        (sort (process-lines "gopkgs") #'string<))
      (setq go-packages-function #'go-packages-gopkgs))
     ((executable-find "fd")
      (defun go-packages-fd ()
        "Return a list of all installed Go packages, using `fd'."
        (sort
         (delete-dups
          (cl-mapcan
           '(lambda (topdir)
              (let ((pkgdir (concat topdir "/pkg/")))
                (--> (shell-command-to-string (concat "fd -e a . " pkgdir))
                     (split-string it "\n")
                     (-map (lambda (str)
	                         (--> (string-remove-prefix pkgdir str)
		                          (string-trim-left it ".*?/")
		                          (string-remove-suffix ".a" it)
		                          )
	                         ) it))))
           (go-root-and-paths)))
         #'string<))
      (setq go-packages-function #'go-packages-fd))))

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c t" . go-tag-add)
                ("C-c T" . go-tag-remove))
    :config (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-t" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run))))

;; Local Golang playground for short snippets
(use-package go-playground
  :diminish
  :commands go-playground-mode)

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
