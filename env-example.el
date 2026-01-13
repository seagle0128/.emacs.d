;;; env.el --- env file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Environment variables.
;;; Code:

;; For LSP performance
;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

;; To avoid loading `exec-path-from-shell' for better performance
;; (setenv "EMACS_PLUS_PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin")
