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

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :diminish
  :functions (gptel-make-openai gptel-make-deepseek gptel-make-anthropic)
  :bind (("C-<f12>"   . gptel)
         ("C-M-<f12>" . gptel-menu))
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  ;; Register backends and setup models
  ;; Securing API keys with authinfo (see `auth-sources')
  ;; format: "machine {HOST} login apikey password {token}"
  (setq gptel-model 'gpt-4o
        gptel-backend
        (gptel-make-openai "Github Models"
          :host "models.inference.ai.azure.com"
          :endpoint "/chat/completions?api-version=2024-05-01-preview"
          :stream t
          :key 'gptel-api-key
          :models '(gpt-4o)))

  (gptel-make-openai "Nvidia"
    :host "integrate.api.nvidia.com"
    :endpoint "/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(z-ai/glm4.7 minimaxai/minimax-m2.1 deepseek-ai/deepseek-v3.1-terminus))

  (gptel-make-openai "ChatGLM"
    :host "open.bigmodel.cn"
    :endpoint "/api/paas/v4/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(glm-4.7 glm-4.7-flash))

  (gptel-make-openai "Moonshot"
    :host "api.moonshot.cn"
    :key 'gptel-api-key
    :stream t
    :models '(kimi-latest kimi-k2-0711-preview))

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key 'gptel-api-key)

  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode
    :commands agent-shell-insert
    :defines magit-mode-map
    :functions (magit-staged-files magit-commit-p magit-thing-at-point)
    :custom (agent-shell-display-action '(display-buffer-reuse-window))
    :bind (("<f12>"      . agent-shell)
           ("<f13>"      . agent-shell)
           ("C-c a"      . agent-shell)
           ("C-c A"      . agent-shell-new-shell)
           :map agent-shell-mode-map
           ("C-h ?"      . agent-shell-help-menu)
           ("C-<return>" . agent-shell-help-menu)
           :map magit-mode-map
           ("C-c C-g"    . my/agent-shell-magit-generate-commit)
           ("C-c C-r"    . my/agent-shell-review-magit-commit))
    :config
    (with-eval-after-load 'magit
      (defun my/agent-shell-magit-generate-commit ()
        "Generate conventional message and commit stage changes in magit."
        (interactive)
        (if (magit-staged-files)
            (agent-shell-insert
             :submit t
             :text "Commit changes with conventional message")
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
