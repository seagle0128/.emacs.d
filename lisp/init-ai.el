;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;; AI configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; FIXME: Eager macro-expansion failure
;; @see https://github.com/karthink/gptel/issues/1272
;; Interact with ChatGPT or other LLMs
(use-package gptel
  :disabled
  :functions gptel-make-openai
  :custom
  (gptel-model 'gpt-4o)
  ;; Put the apikey to `auth-sources'
  ;; Format: "machine {HOST} login {USER} password {APIKEY}"
  ;; The LLM host is used as HOST, and "apikey" as USER.
  (gptel-backend (gptel-make-openai "Github Models"
                   :host "models.inference.ai.azure.com"
                   :endpoint "/chat/completions?api-version=2024-05-01-preview"
                   :stream t
                   :key 'gptel-api-key
                   :models '(gpt-4o))))

;; Generate commit messages for magit
(use-package gptel-magit
  :disabled
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode
    :commands agent-shell-insert
    :functions magit-staged-files magit-commit-p magit-thing-at-point
    :custom (agent-shell-display-action '(display-buffer-reuse-window))
    :bind (("<f12>" . agent-shell-toggle)
           :map magit-mode-map
           ("C-c C-g" . my/agent-shell-magit-generate-message)
           ("C-c C-r" . my/agent-shell-review-magit-commit))
    :config
    (with-eval-after-load 'magit
      (defun my/agent-shell-magit-generate-message ()
        "Generate conventional message and commit stage changes in magit."
        (interactive)
        (if-let ((changes (magit-staged-files)))
            (agent-shell-insert
             :submit t
             :text "Generate conventional messages and commit the stage changes. \
If `git-commit` skill exists, use it")
          (user-error "No staged changes")))

      (defun my/agent-shell-review-magit-commit ()
        "Send the commit from magit to agent-shell for reviews."
        (interactive)
        (if-let ((commit (magit-commit-p (magit-thing-at-point 'git-revision t))))
            (agent-shell-insert
             :submit t
             :text (format "Review commit: %s" commit))
          (user-error "No magit commit at point"))))))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
