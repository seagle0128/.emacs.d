;; init-smartparens.el --- Initialize smartparens configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Smartparens configurations.
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

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :commands sp-with-modes
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)

              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ;; ("C-S-f" . sp-forward-symbol)
              ;; ("C-S-b" . sp-backward-symbol)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)

              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)

              ("C-M-d" . delete-sexp)

              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)

              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)

              ("C-x C-t" . sp-transpose-hybrid-sexp)

              ("C-c ("  . wrap-with-parens)
              ("C-c ["  . wrap-with-brackets)
              ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c _"  . wrap-with-underscores)
              ("C-c `"  . wrap-with-back-quotes))
  :init
  (progn
    (add-hook 'after-init-hook 'smartparens-global-mode)
    (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode))
  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode 1)

    ;; Hydra
    (use-package hydra
      :config
      (bind-key "C-M-s"
                (defhydra hydra-smartparens (:color pink)
                  "Smartparens"
                  ("d" sp-down-sexp "Down")
                  ("e" sp-up-sexp "Up")
                  ("u" sp-backward-up-sexp "Up")
                  ("a" sp-backward-down-sexp "Down")
                  ("f" sp-forward-sexp "Forward")
                  ("b" sp-backward-sexp "Backward")
                  ("n" sp-next-sexp "Next")
                  ("p" sp-previous-sexp "Previous")
                  ("s" sp-split-sexp "Split")
                  ("j" sp-join-sexp "Join")
                  ("k" sp-kill-sexp "Kill" :color blue)
                  ("q" nil "Quit" :color blue))
                smartparens-mode-map))

    ;; Define wrapping functions
    (defmacro def-pairs (pairs)
      `(progn
         ,@(loop for (key . val) in pairs
                 collect
                 `(defun ,(read (concat
                                 "wrap-with-"
                                 (prin1-to-string key)
                                 "s"))
                      (&optional arg)
                    (interactive "p")
                    (sp-wrap-with-pair ,val)))))

    (def-pairs ((paren        . "(")
                (bracket      . "[")
                (brace        . "{")
                (single-quote . "'")
                (double-quote . "\"")
                (back-quote   . "`")))

    ;; Pair Management
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

    ;; Workaround for auto-paring issues for Rails and Django
    (eval-after-load 'web-mode
      (add-hook 'web-mode-hook
                '(lambda ()
                   (sp-local-pair 'web-mode "{" "}" :actions nil)
                   (sp-local-pair 'web-mode "<" ">" :actions nil))))

    ;; Markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*"
                     :wrap "C-*"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC"))
                     :skip-match 'sp--gfm-skip-asterisk)
      (sp-local-pair "**" "**")
      (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

    (defun sp--gfm-skip-asterisk (ms mb me)
      (save-excursion
        (goto-char mb)
        (save-match-data (looking-at "^\\* "))))

    ;; Org-mode
    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    (defun sp--org-skip-asterisk (ms mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))

    ;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">"))

    ;; lisp modes
    (sp-with-modes sp-lisp-modes
      (sp-local-pair "(" nil
                     :wrap "C-("
                     :pre-handlers '(my-add-space-before-sexp-insertion)
                     :post-handlers '(my-add-space-after-sexp-insertion)))

    (defun my-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun my-add-space-before-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (backward-char (length id))
          (when (or (eq (char-syntax (preceding-char)) ?w)
                    (and (looking-back (sp--get-closing-regexp))
                         (not (eq (char-syntax (preceding-char)) ?'))))
            (insert " ")))))

    ;; C
    (sp-with-modes '(malabar-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
    (sp-local-pair 'c-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))
    ;; C++
    (sp-with-modes '(malabar-mode c++-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
    (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                        ("* ||\n[i]" "RET")))
    ))

(provide 'init-smartparens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-smartparens.el ends here
