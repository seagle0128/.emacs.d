;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-
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
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-c M-g" . magit-file-popup))
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Pretty magit http://www.modernemacs.com/post/pretty-magit
  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(prog1
         (add-to-list 'pretty-magit-alist
                      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                            ,ICON ',PROPS))
       (unless ,NO-PROMPT?
         (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  (setq pretty-magit-alist nil)
  (setq pretty-magit-prompt nil)
  (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
  (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
  (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
  (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
  (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
  ;; (pretty-magit "origin"  ? (:box t :height 1.2) t)
  (pretty-magit "master"  ? (:box t :height 1.2) t)
  (pretty-magit "origin"  ? (:box t :height 1.2) t)

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props))))))))

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)

  (with-eval-after-load 'ivy
    (setq use-magit-commit-prompt-p nil)
    (defun use-magit-commit-prompt (&rest args)
      (setq use-magit-commit-prompt-p t))

    (defun magit-commit-prompt ()
      "Magit prompt and insert commit header with faces."
      (interactive)
      (when use-magit-commit-prompt-p
        (setq use-magit-commit-prompt-p nil)
        (insert (ivy-read "Commit Type " pretty-magit-prompt
                          :require-match t :sort t :preselect "Add: "))
        (add-magit-faces)))

    (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
    (add-hook 'git-commit-setup-hook 'magit-commit-prompt)
    (advice-add 'magit-commit :after 'use-magit-commit-prompt)))

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
(use-package git-timemachine)

;; Highlighting regions by last updated time
(use-package smeargle
  :bind (("C-x v S" . smeargle)
         ("C-x v C" . smeargle-commits)
         ("C-x v R" . smeargle-clear)))

;; Git modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; Subversion
(use-package psvn)

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote)

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
