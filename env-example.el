;;; env.el --- env vars setup    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Setup environment variables.
;;;       This file is loaded in early-init.el.
;;; Code:

;; Improve LSP performance
;; Uncomment the following if you are using `lsp-mode'
;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (setenv "LSP_USE_PLISTS" "true")

;; Configure PATH injection for emacs-plus-app
;; Avoid loading `exec-path-from-shell' for performance optimization
;; Uncomment and adjust the following settings as needed
;; @see https://github.com/d12frosted/homebrew-emacs-plus/issues/895
;; (setenv "EMACS_PLUS_PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin")

;;; env.el ends here
