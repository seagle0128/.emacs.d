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
;;
;; API Key management via authinfo (no plain-text secrets):
;;
;;   machine models.inference.ai.azure.com  login api-key password <gh_token>
;;   machine api.deepseek.com               login api-key password <ds_key>
;;   machine dashscope.aliyuncs.com         login api-key password <qwen_key>
;;   machine open.bigmodel.cn               login api-key password <zhipu_key>
;;   machine generativelanguage.googleapis.com login api-key password <gemini_key>
;;   machine api.anthropic.com              login api-key password <claude_key>
;;
(use-package gptel
  :diminish
  :functions (gptel-make-openai gptel-make-anthropic
               gptel-make-deepseek gptel-make-gemini
               gptel-make-ollama)
  :bind (("C-<f12>"   . gptel)
         ("C-M-<f12>" . gptel-menu))
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  ;; GitHub Models (free with GH Copilot subscription)
  (setq gptel-model 'gpt-4.1
        gptel-backend
        (gptel-make-openai "Github Models"
          :host "models.inference.ai.azure.com"
          :endpoint "/chat/completions?api-version=2024-05-01-preview"
          :stream t
          :key 'gptel-api-key
          :models '(gpt-4o gpt-4.1)))

  ;; GLM
  (gptel-make-openai "GLM"
    :host "open.bigmodel.cn"
    :endpoint "/api/paas/v4/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(glm-5.2 glm-5.2-flash glm-4.7 glm-4.7-flash))

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key 'gptel-api-key
    :models '(deepseek-chat deepseek-reasoner))

  ;; Qwen (Alibaba Cloud)
  (gptel-make-openai "Qwen"
    :host "dashscope.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(qwen-plus qwen-turbo-latest qwen-max))

  ;; Gemini (Google)
  (gptel-make-gemini "Gemini"
    :key 'gptel-api-key
    :stream t
    :models '(gemini-2.5-flash gemini-2.5-pro))

  ;; Claude (Anthropic)
  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key
    :models '(claude-sonnet-4-20250514 claude-haiku-3-5-20241022)))

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
           ("C-c C-g"    . centaur-generate-commit)
           ("C-c C-r"    . centaur-review-commit))
    :config
    ;; Integrate into magit
    (with-eval-after-load 'magit
      (defun centaur-generate-commit ()
        "Generate conventional commit message from staged changes."
        (interactive)
        (if (magit-staged-files)
            (agent-shell-insert
             :submit t
             :text "Commit changes with conventional message")
          (user-error "No staged changes")))

      (defun centaur-review-commit ()
        "Send the commit at point to agent-shell for review."
        (interactive)
        (if-let ((commit (magit-commit-p (magit-thing-at-point 'git-revision t))))
            (agent-shell-insert
             :submit t
             :text (format "Review commit: %s" commit))
          (user-error "No magit commit at point"))))))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
