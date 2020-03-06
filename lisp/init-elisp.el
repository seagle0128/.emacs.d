;; init-elisp.el --- Initialize Emacs Lisp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
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

;;; Commentary:
;;
;; Emacs Lisp configurations.
;;

;;; Code:

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :defines (flycheck-disabled-checkers calculate-lisp-indent-last-sexp)
  :functions (helpful-update
              my-lisp-indent-function
              function-advices
              end-of-sexp
              add-button-to-remove-advice
              describe-function-1@advice-remove-button
              helpful-update@advice-remove-button)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-x" . ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer))
  :hook (emacs-lisp-mode . (lambda ()
                             "Disable the checkdoc checker."
                             (setq-local flycheck-disabled-checkers
                                         '(emacs-lisp-checkdoc))))
  :config
  (when (boundp 'elisp-flymake-byte-compile-load-path)
    (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Syntax highlighting of known Elisp symbols
  (use-package highlight-defined
    :hook (emacs-lisp-mode . highlight-defined-mode)
    :init (setq highlight-defined-face-use-itself t))

  ;; Align indent keywords
  ;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
  (defun my-lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local lisp-indent-function #'my-lisp-indent-function)))

  ;; Add remove buttons for advices
  (add-hook 'help-mode-hook 'cursor-sensor-mode)

  (defun function-advices (function)
    "Return FUNCTION's advices."
    (let ((flist (indirect-function function)) advices)
      (while (advice--p flist)
        (setq advices `(,@advices ,(advice--car flist)))
        (setq flist (advice--cdr flist)))
      advices))

  (defun add-button-to-remove-advice (buffer-name function)
    "Add a button to remove advice."
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (save-excursion
          (goto-char (point-min))
          (let ((ad-list (reverse (function-advices function))))
            (while (re-search-forward "^\\(?:This function has \\)?:[-a-z]+ advice: \\(.+\\)\\.?$" nil t)
              (let* ((name (string-trim (match-string 1) "[‘'`]" "[’']"))
                     (symbol (intern-soft name))
                     (advice (or symbol (car ad-list))))
                (when advice
                  (when symbol
                    (cl-assert (eq symbol (car ad-list))))
                  (let ((inhibit-read-only t))
                    (insert "\t")
                    (insert-text-button
                     "[Remove]"
                     'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                     'help-echo (format "%s" advice)
                     'action
                     ;; In case lexical-binding is off
                     `(lambda (_)
                        (when (yes-or-no-p (format "Remove %s ? " ',advice))
                          (message "Removing %s of advice from %s" ',function ',advice)
                          (advice-remove ',function ',advice)
                          (if (eq major-mode 'helpful-mode)
                              (helpful-update)
                            (revert-buffer nil t))))
                     'follow-link t))))
              (setq ad-list (car ad-list))))))))

  (define-advice describe-function-1 (:after (function) advice-remove-button)
    (add-button-to-remove-advice "*Help*" function))

  ;; Remove hook
  (defun remove-hook-at-point ()
    "Remove the hook at the point in the *Help* buffer."
    (interactive)
    (unless (or (eq major-mode 'help-mode)
                (eq major-mode 'helpful-mode)
                (string= (buffer-name) "*Help*"))
      (error "Only for help-mode or helpful-mode"))
    (let ((orig-point (point)))
      (save-excursion
        (when-let
            ((hook (progn (goto-char (point-min)) (symbol-at-point)))
             (func (when (and
                          (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                              (goto-char orig-point))
                          (sexp-at-point))
                     (end-of-sexp)
                     (backward-char 1)
                     (catch 'break
                       (while t
                         (condition-case _err
                             (backward-sexp)
                           (scan-error (throw 'break nil)))
                         (let ((bounds (bounds-of-thing-at-point 'sexp)))
                           (when (<= (car bounds) orig-point (cdr bounds))
                             (throw 'break (sexp-at-point)))))))))
          (when (yes-or-no-p (format "Remove %s from %s? " func hook))
            (remove-hook hook func)
            (if (eq major-mode 'helpful-mode)
                (helpful-update)
              (revert-buffer nil t)))))))
  (bind-key "r" #'remove-hook-at-point help-mode-map))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish)

;; Interactive macro expander
(use-package macrostep
  :custom-face
  (macrostep-expansion-highlight-face ((t (:background ,(face-background 'tooltip) :extend t))))
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c e" . macrostep-expand))
  :config
  (add-hook 'after-load-theme-hook
            (lambda ()
              (set-face-background 'macrostep-expansion-highlight-face
                                   (face-background 'tooltip)))))

;; A better *Help* buffer
(use-package helpful
  :defines (counsel-describe-function-function
            counsel-describe-variable-function)
  :commands helpful--buffer
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ("C-c C-d" . helpful-at-point)
         :map helpful-mode-map
         ("r" . remove-hook-at-point))
  :hook (helpful-mode . cursor-sensor-mode) ; for remove-advice button
  :init
  (with-eval-after-load 'counsel
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable))

  (with-eval-after-load 'apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  ;; Add remove buttons for advices
  (define-advice helpful-update (:after () advice-remove-button)
    (when helpful--callable-p
      (add-button-to-remove-advice (helpful--buffer helpful--sym t) helpful--sym)))
  :config
  (with-no-warnings
    ;; Open the buffer in other window
    (defun my-helpful--navigate (button)
      "Navigate to the path this BUTTON represents."
      (find-file-other-window (substring-no-properties (button-get button 'path)))
      ;; We use `get-text-property' to work around an Emacs 25 bug:
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
      (-when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
        (helpful--goto-char-widen pos)))
    (advice-add #'helpful--navigate :override #'my-helpful--navigate)))

;; For ERT
(use-package overseer
  :diminish
  :hook (emacs-lisp-mode . overseer-mode))

(provide 'init-elisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elisp.el ends here
