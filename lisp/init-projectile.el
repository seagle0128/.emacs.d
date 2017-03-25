;; init-projectile.el --- Initialize projectile configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.2.0
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

;; Manage and navigate projects
(use-package projectile
  :bind (("C-S-t" . projectile-find-file)
         ("s-t" . projectile-find-file))
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config
  ;; FIXME: tramp issues https://github.com/bbatsov/projectile/pull/1129
  (defun projectile-project-root ()
    "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
    (let* ((dir default-directory)
           (is-local (not (file-remote-p dir)))
           (is-connected (file-remote-p dir nil t)))
      (or (when (or is-local is-connected)
            (cl-some
             (lambda (func)
               (let* ((cache-key (format "%s-%s" func dir))
                      (cache-value (gethash cache-key projectile-project-root-cache)))
                 (if (and cache-value (file-exists-p cache-value))
                     cache-value
                   (let ((value (funcall func (file-truename dir))))
                     (puthash cache-key value projectile-project-root-cache)
                     value))))
             projectile-project-root-files-functions))
          (if projectile-require-project-root
              (error "You're not in a project")
            default-directory))))

  (setq projectile-mode-line
        '(:eval
          (if (file-remote-p default-directory)
              ""
            (format " [%s]"
                    (projectile-project-name)))))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val))

  ;; Use ag instead of find in a generic project
  (when (executable-find "ag")
    (let ((val (concat "ag -U -l --nocolor"
                       (mapconcat 'identity
                                  (cons "" projectile-globally-ignored-directories)
                                  " --ignore-dir=")
                       " -g . | tr '\\n' '\\0'")))
      (setq projectile-generic-command val)))

  ;; Rails project
  (use-package projectile-rails
    :diminish projectile-rails-mode
    :init (projectile-rails-global-mode 1)))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
