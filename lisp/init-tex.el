;;; Code :




(require 'init-const)
(require 'init-custom)

;(use-package pdf-tools
;  :pin manual ;; manually update
;  :config
  ;; initialise
  ;(pdf-tools-install)
  ;; open pdfs scaled to fit page
  ;(setq-default pdf-view-display-size 'fit-page)
  ;)
  
 ;(pdf-loader-install)

;(require 'ox-latex)
;(require 'ox-beamer)


(use-package ob-latex                                      ;
  ;;  :straight t
  :ensure nil
  :after org
  :defer
  :custom (org-latex-compiler "lualatex"))




(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))


(with-eval-after-load "ox-latex"
(add-to-list 'org-latex-classes
             '("koma-article" "\\documentclass{scrartcl}
                            [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;
;; Custom classes
;;;
(add-to-list 'org-latex-classes
             '("koma-book" "\\documentclass{scrbook}
                    [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(add-to-list 'org-latex-classes
             '("memoir-article"
               "\\documentclass[12pt,oneside,article]{memoir}
                [PACKAGES]
                [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )






(provide 'init-tex)
