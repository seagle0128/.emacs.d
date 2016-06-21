;; Robot mode
;; ==========
;;
;; A major mode for editing robot framework text files.
;; Add the following to your .emacs file
;;
;;    (load-file "path/to/robot-mode.el")
;;    (add-to-list 'auto-mode-alist '("\\.txt\\'" . robot-mode))
;;
;; Type "M-x load-file" and give the path to the .emacs file (e.g. ~/.emacs)
;; to reload the file. Now when you open a .txt file emacs automatically sets
;; the robot-mode on for that buffer. This will also be done automatically when
;; you start emacs.
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; You can participate by sending pull requests to https://github.com/sakari/robot-mode

(setq robot-mode-keywords
      '(
    ;;normal comment
    ("#.*" . font-lock-comment-face)
    ;;Section headers
    ("\\*\\*\\* [^\\*]+ \\*\\*\\*" . font-lock-keyword-face)
    ;;keyword definitions
    ("^[^ \t\n].+" . font-lock-function-name-face)
    ;;Variables
    ("\\(\\$\\|@\\){\\( ?[^ }$]\\)+}" 0 font-lock-variable-name-face t)
    ;;tags etc
    ("\\[[^\]]+\\]+" . font-lock-constant-face)
    ;;comment kw
    ("comment  .*" . font-lock-comment-face)
    )
      )

(defun robot-indent()
  "Returns the string used in indation.
Set indent-tabs-mode to non-nil to use tabs in indentation. If indent-tabs-mode is
set to nil c-basic-offset defines how many spaces are used for indentation. If c-basic-offset is
not set 4 spaces are used.
 "
 (if indent-tabs-mode
     "\t"
   (make-string (if (boundp 'c-basic-offset)
            c-basic-offset 4)
        ?\ )
   )
 )


(defun robot-mode-kw-at-point()
  "Return the robot keyword (or possibly infix variable) around the current point in buffer"
  (defun extract-kw (str)
    (defun trim (str)
      (replace-regexp-in-string "\\(^\s+\\)\\|\\(\s+$\\)\\|\n$" "" str))
    (defun cut-kw (str)
      (replace-regexp-in-string "  .*$" "" str))
    (defun cut-bdd (str)
      (replace-regexp-in-string "^\\(given\\)\\|\\(then\\)\\|\\(when\\)\\s*" "" str))
    (cut-kw (cut-bdd (trim str)))
    )
  (let* ((kw-end (save-excursion (re-search-forward "$\\|\\(  \\)")))
     (kw-start (save-excursion (re-search-backward "^\\|\\(  \\)")))
     )
    (save-excursion
      (let* ((variable-end (re-search-forward "[^}]*}" kw-end t))
         (variable-start (re-search-backward "\\(\\$\\|@\\){[^{]*" kw-start t)))
    (if (and variable-end variable-start)
        (buffer-substring variable-start variable-end)
      (extract-kw (buffer-substring kw-start kw-end))
      )
    )
      )
    )
  )

(defun robot-mode-continue-find-kw()
  "Find the next matching robot kw."
  (interactive)
  (find-tag-regexp "" t)
  )

(defun robot-mode-make-kw-regexp(kw)
  (defun match-underscores (str)
    (replace-regexp-in-string "\\(_\\| \\)" "[_ ]?" str t t))
  (defun match-infix-args (str)
    (replace-regexp-in-string "'[^']+'" "'\\($\\|@\\){[^}]+}'" str t t))
  (match-infix-args (match-underscores kw))
)

(defun robot-mode-find-first-kw()
  "Start the robot kw search."
  (setq default-kw (if (and transient-mark-mode mark-active)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (robot-mode-kw-at-point)
            ))
  (let ((kw (read-from-minibuffer (format "Find kw (%s): " default-kw))))
    (if (string= "" kw) (find-tag-regexp (robot-mode-make-kw-regexp default-kw))
      (find-tag-regexp (robot-mode-make-kw-regexp kw))
      )
    )
  )

(defun robot-mode-complete(kw-prefix)
  "Complete the symbol before point.

\\<robot-mode-map>
This function is bound to \\[robot-mode-complete].
"
  (interactive (list (robot-mode-kw-at-point)))
  (let ((kw-regexp (robot-mode-make-kw-regexp kw-prefix)))
    (defun normalize-candidate-kw(kw)
      (replace-regexp-in-string "_" " " kw)
      )
    (let ((possible-completions ()))
      (let ((enable-recursive-minibuffers t)
        (pick-next-buffer nil)
        (kw-full (format "^ *\\(def +\\)?\\([^\177 \n]*%s[^\177\n]*?\\)(?\177\\(\\(.+\\)\\)?" kw-regexp)))
    (save-excursion
      (visit-tags-table-buffer pick-next-buffer)
      (set 'pick-next-buffer t)
      (goto-char (point-min))
      (while (re-search-forward kw-full nil t)
        (if (or (match-beginning 2) (match-beginning 4))
        (let ((got (buffer-substring
                (or (match-beginning 4) (match-beginning 2))
                (or (match-end 4) (match-end 2)))))
          (add-to-list 'possible-completions (normalize-candidate-kw got) )
          )
          )
        )
      )
    )
      (cond ((not possible-completions) (message "No completions found!"))
        ((= (length possible-completions) 1)
         (insert (substring (car possible-completions) (length kw-prefix))))
        (t (with-output-to-temp-buffer "*Robot KWs*"
           (display-completion-list possible-completions kw-prefix))
           )
        )
      )
    )
  )

(defun robot-mode-find-kw(continue)
  "Find the kw in region or in the line where the point is from TAGS.

If 'continue' is is non nil or interactively if the function is called
with a prefix argument (i.e. prefixed with \\[universal-argument]) then continue from the last
found kw.

\\<robot-mode-map>
This function is bound to \\[robot-mode-find-kw].
"
  (interactive "P")
  (if continue (robot-mode-continue-find-kw)
    (robot-mode-find-first-kw)
    )
  )

(defun robot-mode-newline()
"Do the right thing when inserting newline.

\\<robot-mode-map>
This function is bound to \\[robot-mode-newline].
"
(interactive)

(defun inside-kw-definition()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^ \t]" (line-end-position) t)
    )
  )

(defun remove-possible-empty-tab()
  (save-excursion
    (beginning-of-line)
    (let ((line (delete-and-extract-region (line-beginning-position) (line-end-position)))
      )
      (insert (replace-regexp-in-string "^[ \t]+$" "" line))
      )
    )
  )

(if (inside-kw-definition) (insert (concat "\n" (robot-indent)))
  (remove-possible-empty-tab)
  (insert "\n")
  )
)

(defun robot-mode-indent-region()
"Fix indentation in the region.

\\<robot-mode-map>
This function is bound to \\[robot-mode-newline].
"
(interactive)
(save-excursion
  (let* ((region (delete-and-extract-region (region-beginning) (region-end) ))
     (fixed-region (replace-regexp-in-string "^[ \t]+" (robot-indent) region))
     )
    (insert fixed-region)
    )
  )
)

(defun robot-mode-indent()
"Switch between indent and unindent in robot mode.

\\<robot-mode-map>
This function is bound to \\[robot-mode-indent].
"
(interactive)
(save-excursion
  (beginning-of-line)
  (let ((line (delete-and-extract-region (line-beginning-position) (line-end-position)))
        )
    (if (string-match "^[ \t]+" line)
        (insert (replace-regexp-in-string "^[ \t]+" "" line))
      (insert (concat (robot-indent) line))
      )
    )
  )
)

;;;###autoload
(define-derived-mode robot-mode prog-mode
  "robot mode"
  "Major mode for editing Robot Framework text files.

This mode rebinds the following keys to new function:
\\{robot-mode-map}
In the table above <remap> <function> means that the function is bound to whatever
key <function> was bound previously. To see the actual key binding press enter on
top of the bound function.

You can use \\[beginning-of-defun] to move to the beginning of the kw
the cursor point is at and \\[end-of-defun] to move to the end of the kw.
To select (i.e. put a region around) the whole kw definition press \\[mark-defun].

Set indent-tabs-mode to non-nil to use tabs for indantation. If indent-tabs-mode is nil,
c-basic-offset defines the amount of spaces that are inserted when indenting.
"
  (require 'etags)
  (set (make-local-variable 'font-lock-defaults) '(robot-mode-keywords))

  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#")

  (set (make-local-variable 'beginning-of-defun-function) (lambda()
                                                            (re-search-backward "^[^ \t\n]")
                                                            )
       )
  (set (make-local-variable 'end-of-defun-function) (lambda()
                                                      (end-of-line)
                                                      (if (not (re-search-forward "^[^ \t\n]" nil t))
                                                          (goto-char (point-max))
                                                        (beginning-of-line)
                                                        )
                                                      )
       )


  (define-key robot-mode-map (kbd "TAB") 'robot-mode-indent)
  (define-key robot-mode-map (kbd "RET") 'robot-mode-newline)
  (define-key robot-mode-map [remap find-tag] 'robot-mode-find-kw)
  (define-key robot-mode-map [remap complete-symbol] 'robot-mode-complete)
  (define-key robot-mode-map [remap indent-region] 'robot-mode-indent-region)
  )

(provide 'robot-mode)
