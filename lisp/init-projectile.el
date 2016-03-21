;; init-projectile.el --- Initialize projectile configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
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

(use-package projectile
  :bind ("C-S-t" . projectile-find-file)
  :config
  (projectile-global-mode 1)
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-project-root-files-functions
  ;;       '(projectile-root-top-down
  ;;         projectile-root-top-down-recurring
  ;;         projectile-root-bottom-up))

  (add-to-list 'projectile-project-root-files-bottom-up ".p4config")

  (when (executable-find "ag")
    (let ((val (concat "ag -U -l --nocolor"
                       (mapconcat 'identity (cons "" projectile-globally-ignored-directories) " --ignore-dir=")
                       " -g . | tr '\\n' '\\0'")))
      (setq projectile-generic-command val)))

  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))))

;; Rails
(use-package projectile-rails
  :init (add-hook 'projectile-mode-hook 'projectile-rails-on))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
