;; Scala configurations, based on scala metals https://scalameta.org/metals/
;; It works best with bloop server: https://scalacenter.github.io/bloop/

(use-package scala-mode
  :hook (scala-mode . lsp)
  :mode ("\\.s\\(cala\\|bt\\)$"    . scala-mode))

(provide 'init-scala)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-scala.el ends here
