;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.1.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Projectile configurations.
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

(eval-when-compile (require 'init-const))

;; Manage and navigate projects
(use-package projectile
  :bind (("C-S-t" . projectile-find-file) ; for Windows
         ("s-t" . projectile-find-file))  ; Cmd-t for Mac and Super-t for Linux
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  (setq projectile-mode-line
        '(:eval (format "[%s]" (projectile-project-name))))

  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)

  ;; Use the faster searcher to handle project files:
  ;; ripgrep `rg', the platinum searcher `pt' or the silver searcher `ag'
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -0 -l --color=never --hidden --sort-files")
          ((executable-find "pt")
           "pt -0 -l --nocolor --hidden")
          ((executable-find "ag")
           (concat "ag -0 -l --nocolor --hidden"
                   (mapconcat #'identity
                              (cons "" projectile-globally-ignored-directories)
                              " --ignore-dir="))))))
    (setq projectile-generic-command command))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "rg") (executable-find "pt") (executable-find "ag"))
      (setq projectile-indexing-method 'alien))
    (setq projectile-git-submodule-command ""))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val))

  ;; Rails project
  (use-package projectile-rails
    :diminish projectile-rails-mode
    :init (projectile-rails-global-mode 1)))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
