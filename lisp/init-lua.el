;; init-lua.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

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
;; lua configuration.
;;

;;; Code:

;; lua Mode
(use-package lua-mode)

;; use an indentation width of two spaces
(setq lua-indent-level 4)

;; Add dangling '(', remove '='
(setq lua-cont-eol-regexp
      (eval-when-compile
        (concat
         "\\((\\|\\_<"
         (regexp-opt '("and" "or" "not" "in" "for" "while"
                       "local" "function") t)
         "\\_>\\|"
         "\\(^\\|[^" "]\\)"
         (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "<" ">" "<=" ">=" "~=") t)
         "\\)"
         "\\s *\\=")))

(defun lua-calculate-indentation (&optional parse-start)
  "Overwrites the default lua-mode function that calculates the
column to which the current line should be indented to."
  (save-excursion
    (when parse-start
      (goto-char parse-start))

    ;; We calculate the indentation column depending on the previous
    ;; non-blank, non-comment code line. Also, when the current line
    ;; is a continuation of that previous line, we add one additional
    ;; unit of indentation.
    (+ (if (lua-is-continuing-statement-p) lua-indent-level 0)
       (if (lua-goto-nonblank-previous-line)
           (+ (current-indentation) (lua-calculate-indentation-right-shift-next))
         0))))

(defun lua-calculate-indentation-right-shift-next (&optional parse-start)
  "Assuming that the next code line is not a block ending line,
this function returns the column offset that line should be
indented to with respect to the current line."
  (let ((eol)
        (token)
        (token-info)
        (shift 0))
    (save-excursion
      (when parse-start
        (goto-char parse-start))

      ;; count the balance of block-opening and block-closing tokens
      ;; from the beginning to the end of this line.
      (setq eol (line-end-position))
      (beginning-of-line)
      (while (and (lua-find-regexp 'forward lua-indentation-modifier-regexp)
                  (<= (point) eol)
                  (setq token (match-string 0))
                  (setq token-info (assoc token lua-block-token-alist)))
        ;; we found a token. Now, is it an opening or closing token?
        (if (eq (nth 2 token-info) 'open)
            (setq shift (+ shift lua-indent-level))
          (when (or (> shift 0)
                    (string= token ")"))
            (setq shift (- shift lua-indent-level))))))
    shift))

(add-hook 'lua-mode-hook
          (lambda () (unless (fboundp 'lua-calculate-indentation-right-shift-next))))

(provide 'init-lua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lua.el ends here
