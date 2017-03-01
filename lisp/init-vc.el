;; init-vc.el --- Initialize scm configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.1.0
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
  :defer t
  :init (add-hook 'after-init-hook 'global-magit-file-mode)
  :config (setenv "GIT_ASKPASS" "git-gui--askpass"))

(use-package git-timemachine :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)

;; Subversion
(use-package psvn
  :defer t
  :bind (("C-x p" . svn-status)))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote :defer t)

(provide 'init-vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vc.el ends here
