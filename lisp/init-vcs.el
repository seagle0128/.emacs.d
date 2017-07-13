;; init-vcs.el --- Initialize version control system configurations.
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
;;             Version control systems, e.g. Git, SVN.
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

(require 'init-const)

;; Git
(use-package magit
  :init (add-hook 'after-init-hook 'global-magit-file-mode)
  :config
  (setenv "GIT_ASKPASS" "git-gui--askpass")

  ;; FIXME: Workaround for
  ;; https://github.com/dgutov/diff-hl/issues/85
  ;; https://github.com/magit/magit/issues/3014
  ;; (magit-auto-revert-mode -1)
  (defun magit-turn-on-auto-revert-mode-if-desired (&optional file)
    (if file
        (--when-let (find-buffer-visiting file)
          (with-current-buffer it
            (magit-turn-on-auto-revert-mode-if-desired)))
      (when (and buffer-file-name
                 (file-readable-p buffer-file-name)
                 (magit-toplevel)
                 (or (not magit-auto-revert-tracked-only)
                     (magit-file-tracked-p buffer-file-name)))
        (unless (or auto-revert-mode global-auto-revert-mode)
          (auto-revert-mode))))))

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :diminish magit-gitflow-mode
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; Git-Svn extension for Magit
(use-package magit-svn
  :diminish magit-svn-mode
  :init (add-hook 'magit-mode-hook 'magit-svn-mode))

;;; Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer-install t
  :commands git-timemachine)

(use-package gitconfig-mode)
(use-package gitignore-mode)

;; Subversion
(use-package psvn
  :defer-install t
  :commands (svn-status svn-examine svn-checkout))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :defer-install t
  :commands (bar-browse bar-to-clipboard))

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
