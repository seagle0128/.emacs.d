;;; doom-modeline.el --- A minimal modeline from DOOM.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/doom-modeline
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (all-the-icons "1.0.0") (projectile "0.10.0") (shrink-path "0.2.0") (eldoc-eval "0.1") (dash "2.11.0"))
;; Keywords: faces

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
;; This package offers a modern modeline them which is extraced from DOOM Emacs
;; (https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).
;;
;; It's also the part of Centaur Emacs (https://github.com/seagle0128/.emacs.d).
;;
;; The DOOM modeline was designed for minimalism, and offers:
;; 1. A match count panel (for evil-search, iedit and evil-substitute)
;; 2. An indicator for recording a macro
;; 3. Local python/ruby version in the major-mode
;; 4. A customizable mode-line height (see doom-modeline-height)
;; 5. An error/warning count segment for flycheck
;;

;;; Code:

(require 'all-the-icons)
(require 'eldoc-eval)
(require 'projectile)
(require 'shrink-path)

;;
;; Variables
;;

(defvar doom-modeline-height 29
  "How tall the mode-line should be (only respected in GUI Emacs).")

(defvar doom-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI Emacs).")

(defvar doom-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `doom-modeline-buffer-file-name'.

  Given ~/Projects/FOSS/emacs/lisp/comint.el
  truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  truncate-upto-root => ~/P/F/e/lisp/comint.el
  truncate-all => ~/P/F/e/l/comint.el
  relative-from-project => emacs/lisp/comint.el
  relative-to-project => lisp/comint.el
  file-name => comint.el")

;; externs
(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--total-matched)
(defvar anzu-cons-mode-line-p)
(defvar anzu-minimum-input-length)
(defvar anzu-search-threshold)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-mode)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar flycheck-current-errors)
(defvar iedit-mode)
(defvar iedit-occurrences-overlays)
(defvar text-scale-mode-amount)
;;
;; Custom faces
;;

(defgroup doom-modeline nil
  "Doom mode-line faces."
  :group 'faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path.")

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path.")

(defface doom-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `doom-modeline--anzu', `doom-modeline--evil-substitute' and
`iedit'")

(defface doom-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface doom-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface doom-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'")

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active.")

(defface doom-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface doom-modeline-eyebrowse '((t ()))
  "The face used for eyebrowse.")

(eval-and-compile
  (defun doom-modeline--resolve-hook-forms (hooks)
    (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
             for hook in (doom-modeline-enlist (doom-modeline-unquote hooks))
             if (eq (car-safe hook) 'quote)
             collect (cadr hook)
             else if quoted-p
             collect hook
             else collect (intern (format "%s-hook" (symbol-name hook)))))

  (defun doom-modeline-enlist (exp)
    "Return EXP wrapped in a list, or as-is if already a list."
    (if (listp exp) exp (list exp)))

  (defun doom-modeline-unquote (exp)
    "Return EXP unquoted."
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp)

  (defvar doom-modeline--transient-counter 0))


;;
;; Modeline library
;;

(eval-and-compile
  (defvar doom-modeline-fn-alist ())
  (defvar doom-modeline-var-alist ()))

(defmacro doom-modeline-def-segment (name &rest body)
  "Defines a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           (add-to-list 'doom-modeline-var-alist (cons name (car body)))
           `(add-to-list 'doom-modeline-var-alist (cons ',name ',(car body))))
          (t
           (add-to-list 'doom-modeline-fn-alist (cons name sym))
           `(progn
              (fset ',sym (lambda () ,docstring ,@body))
              (add-to-list 'doom-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (byte-compile #',sym))))))))

(defsubst doom-modeline--prepare-segments (segments)
  "Prepare mode-line `SEGMENTS'."
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (cdr (assq seg doom-modeline-fn-alist)))
                    (push (list it) forms))
                   ((setq it (cdr (assq seg doom-modeline-var-alist)))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defmacro doom-modeline-def-modeline (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it.

  NAME is a symbol to identify it (used by `doom-modeline' for retrieval).
  LHS and RHS are lists of symbols of modeline segments defined with
  `doom-modeline-def-segment'.
  Example:
  (doom-modeline-def-modeline minimal
    (bar matches \" \" buffer-info)
    (media-info major-mode))
  (doom-modeline-set-modeline 'minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom-modeline--prepare-segments lhs))
        (rhs-forms (doom-modeline--prepare-segments rhs)))
    `(progn
       (fset ',sym
             (lambda ()
               ,(concat "Modeline:\n"
                        (format "  %s\n  %s"
                                (prin1-to-string lhs)
                                (prin1-to-string rhs)))
               (let ((lhs (list ,@lhs-forms))
                     (rhs (list ,@rhs-forms)))
                 (let ((rhs-str (format-mode-line rhs)))
                   (list lhs
                         (propertize
                          " " 'display
                          `((space :align-to (- (+ right right-fringe right-margin)
                                                ,(+ 1 (string-width rhs-str))))))
                         rhs-str)))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun doom-modeline (key)
  "Return a mode-line configuration associated with KEY (a symbol).

  Throws an error if it doesn't exist."
  (let ((fn (intern (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun doom-modeline-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.

  If DEFAULT is non-nil, set the default mode-line for all buffers."
  (let ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))

(defun doom-modeline-project-root ()
  "Get the path to the root of your project.

  If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))

;;
;; modeline configs
;;

(defun doom-modeline-eldoc (text)
  "Get eldoc TEXT for mode-line."
  (concat (when (display-graphic-p)
            (doom-modeline--make-xpm 'doom-modeline-eldoc-bar
                                     doom-modeline-height
                                     doom-modeline-bar-width))
          text))

;; Show eldoc in the mode-line with `eval-expression'
(defun doom-modeline--show-eldoc (input)
  "Display string INPUT in the mode-line next to minibuffer."
  (with-current-buffer (eldoc-current-buffer)
    (let* ((str              (and (stringp input) input))
           (mode-line-format (or (and str (or (doom-modeline-eldoc str) str))
                                 mode-line-format))
           mode-line-in-non-selected-windows)
      (force-mode-line-update)
      (sit-for eldoc-show-in-mode-line-delay))))
(setq eldoc-in-minibuffer-show-fn #'doom-modeline--show-eldoc)

(eldoc-in-minibuffer-mode +1)


;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
(when (featurep 'evil-anzu)
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)

  (defun doom-modeline-fix-anzu-count (positions here)
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))
  (advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

  ;; Avoid anzu conflicts across buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state
                              anzu--cached-count anzu--cached-positions anzu--last-command
                              anzu--last-isearch-string anzu--overflow-p))

  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  ;; (add-hook '+evil-esc-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status))


;; Keep `doom-modeline-current-window' up-to-date
(defvar doom-modeline-current-window (frame-selected-window))
(defun doom-modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq doom-modeline-current-window win)
      (force-mode-line-update))))

(defun doom-modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq doom-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'doom-modeline-set-selected-window)
(advice-add #'handle-switch-frame :after #'doom-modeline-set-selected-window)
(advice-add #'select-window :after #'doom-modeline-set-selected-window)
(with-no-warnings
  (if (not (boundp 'after-focus-change-function))
      (progn
        (add-hook 'focus-in-hook  #'doom-modeline-set-selected-window)
        (add-hook 'focus-out-hook #'doom-modeline-unset-selected-window))
    (defun doom-modeline-refresh-frame ()
      (setq doom-modeline-current-window nil)
      (cl-loop for frame in (frame-list)
               if (eq (frame-focus-state frame) t)
               return (setq doom-modeline-current-window (frame-selected-window frame)))
      (force-mode-line-update))
    (add-function :after after-focus-change-function #'doom-modeline-refresh-frame)))

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local doom-modeline-env-version nil)
(defvar-local doom-modeline-env-command nil)
(add-hook 'focus-in-hook #'doom-modeline-update-env)
(add-hook 'find-file-hook #'doom-modeline-update-env)
(defun doom-modeline-update-env ()
  "Update environment info on mode-line."
  (when doom-modeline-env-command
    (let* ((default-directory (doom-modeline-project-root))
           (s (shell-command-to-string doom-modeline-env-command)))
      (setq doom-modeline-env-version (if (string-match "[ \t\n\r]+\\'" s)
                                          (replace-match "" t t s)
                                        s)))))
;;
;; Modeline helpers
;;

(defun doom-modeline-maybe-icon-octicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-octicon args)))

(defun doom-modeline-maybe-icon-faicon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-faicon args)))

(defun doom-modeline-maybe-icon-material (&rest args)
  "Display material icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-material args)))

(defsubst doom-modeline--active ()
  "Whether is an active window."
  (eq (selected-window) doom-modeline-current-window))

(defun doom-modeline--make-xpm (face width height)
  "Create an XPM bitmap via FACE, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun doom-modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name' based on `+doom-modeline-buffer-file-name-style'."
  (let ((buffer-file-name (or buffer-file-name ""))
        (buffer-file-truename (or buffer-file-truename "")))
    (propertize
     (pcase doom-modeline-buffer-file-name-style
       (`truncate-upto-project (doom-modeline--buffer-file-name 'shrink))
       (`truncate-upto-root (doom-modeline--buffer-file-name-truncate))
       (`truncate-all (doom-modeline--buffer-file-name-truncate t))
       (`relative-to-project (doom-modeline--buffer-file-name-relative))
       (`relative-from-project (doom-modeline--buffer-file-name-relative 'include-project))
       (`file-name (propertize (file-name-nondirectory buffer-file-name)
                               'face
                               (let ((face (or (and (buffer-modified-p)
                                                    'doom-modeline-buffer-modified)
                                               (and (active)
                                                    'doom-modeline-buffer-file))))
                                 (when face `(:inherit ,face))))))
     'help-echo buffer-file-truename)))

(defun doom-modeline--buffer-file-name-truncate (&optional truncate-tail)
  "Propertized variable `buffer-file-name' that truncates every dir along path.

  If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename)))
        (active (doom-modeline--active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'doom-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory buffer-file-name)
                              'face (if file-faces `(:inherit ,file-faces)))))))))

(defun doom-modeline--buffer-file-name-relative (&optional include-project)
  "Propertized variable `buffer-file-name' showing directories relative to INCLUDE-PROJECT root only."
  (let ((root (doom-modeline-project-root))
        (active (doom-modeline--active)))
    (if (null root)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
             (true-filename (file-truename buffer-file-name))
             (relative-dirs (file-relative-name (file-name-directory true-filename)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory true-filename)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun doom-modeline--buffer-file-name (truncate-project-root-parent)
  "Propertized variable `buffer-file-name'.

  If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
  fish-shell style.

  Example:
  ~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (doom-modeline-project-root))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-name)
                                                  buffer-file-name))
         (active (doom-modeline--active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,filename) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'doom-modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize (if truncate-project-root-parent
                                      root-path-parent
                                    (abbreviate-file-name project-root))
                                  'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize filename 'face file-props)))))))))


;;
;; buffer information
;;

(doom-modeline-def-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (doom-modeline--active) 'doom-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (doom-modeline-maybe-icon-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (doom-modeline-maybe-icon-octicon
                          "lock"
                          :face 'doom-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (doom-modeline-maybe-icon-faicon
                          "floppy-o"
                          :face 'doom-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (doom-modeline-maybe-icon-octicon
                          "circle-slash"
                          :face 'doom-modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (doom-modeline-maybe-icon-octicon
                          "fold"
                          :face 'doom-modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (doom-modeline-buffer-file-name)
            "%b")))

(doom-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'doom-modeline-buffer-modified)
               ((doom-modeline--active) 'doom-modeline-buffer-file))))

;;
(doom-modeline-def-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

;;
;; major-mode
;;

(doom-modeline-def-segment major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (when doom-modeline-env-version
             (concat " " doom-modeline-env-version))
           (and (featurep 'face-remap)
                (> text-scale-mode-amount 0)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))


;;
;; vcs
;;

(defvar-local doom-modeline--vcs nil)
(defun doom-modeline--update-vcs ()
  "Update vsc state in mode-line."
  (setq doom-modeline--vcs
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (let ((face    'mode-line-inactive)
                  (active  (doom-modeline--active))
                  (all-the-icons-default-adjust -0.1))
              (concat (if (display-graphic-p) "  ")
                      (cond ((memq state '(edited added))
                             (if active (setq face 'doom-modeline-info))
                             (doom-modeline-maybe-icon-octicon
                              "git-compare"
                              :face face
                              :v-adjust -0.05))
                            ((eq state 'needs-merge)
                             (if active (setq face 'doom-modeline-info))
                             (doom-modeline-maybe-icon-octicon "git-merge" :face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'doom-modeline-warning))
                             (doom-modeline-maybe-icon-octicon "arrow-down" :face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'doom-modeline-urgent))
                             (doom-modeline-maybe-icon-octicon "alert" :face face))
                            (t
                             (if active (setq face 'font-lock-doc-face))
                             (doom-modeline-maybe-icon-octicon
                              "git-branch"
                              :face face
                              :v-adjust -0.05)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      " "))))))
(add-hook 'after-save-hook #'doom-modeline--update-vcs)
(add-hook 'find-file-hook #'doom-modeline--update-vcs t)

(declare-function magit-toplevel "magit-git")
(defun doom-modeline-magit-post-refresh ()
  "Update vcs state in mode-line after refreshing in magit."
  (dolist (buf (buffer-list))
    (when (and (not (buffer-modified-p buf))
               (buffer-file-name buf)
               (file-exists-p (buffer-file-name buf))
               (file-in-directory-p (buffer-file-name buf) (magit-toplevel)))
      (with-current-buffer buf
        (vc-refresh-state)
        (doom-modeline--update-vcs)))))
(add-hook 'magit-post-refresh-hook #'doom-modeline-magit-post-refresh)

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  doom-modeline--vcs)


;;
;; flycheck
;;

(defvar doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "Text style with icons in mode-line.")

(defun doom-modeline-icon (icon &optional text face voffset)
  "Displays an ICON with FACE, followed by TEXT.
Uses `all-the-icons-material' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (doom-modeline-maybe-icon-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text doom-modeline-vspc)))
          (if text (propertize text 'face face))
          (if vc-mode "  " " ")))

(defvar-local doom-modeline--flycheck nil)
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-segment)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-segment)

(defun doom-modeline-update-flycheck-segment (&optional status)
  "Update flycheck segment via STATUS."
  (setq doom-modeline--flycheck
        (pcase status
          (`finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (doom-modeline-icon "do_not_disturb_alt"
                                                 (number-to-string sum)
                                                 (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                                 -0.25)))
                       (doom-modeline-icon "check" nil 'doom-modeline-info)))
          (`running     (doom-modeline-icon "access_time" nil 'font-lock-doc-face -0.25))
          (`no-checker  (doom-modeline-icon "sim_card_alert" "-" 'font-lock-doc-face))
          (`errored     (doom-modeline-icon "sim_card_alert" "Error" 'doom-modeline-urgent))
          (`interrupted (doom-modeline-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(doom-modeline-def-segment flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  doom-modeline--flycheck)


;;
;; selection-info
;;

(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local doom-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(doom-modeline-def-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and mark-active (doom-modeline--active))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-state) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (doom-modeline-column end)
                                            (doom-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when doom-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'doom-modeline-highlight))))

;;
;; matches (anzu, evil-substitute, iedit, macro)
;;

(defun doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (doom-modeline--active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-panel)
              sep
              (doom-modeline-maybe-icon-octicon "triangle-right"
                                                :face 'doom-modeline-panel
                                                :v-adjust -0.05)
              sep))))

(defsubst doom-modeline--anzu ()
  "Show the match index and total number thereof.

  Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (setq anzu-cons-mode-line-p nil)
  (when (and (featurep 'anzu)
             anzu--state
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(defsubst doom-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (featurep 'evil)
             evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(defun doom-modeline-themes--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defsubst doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (featurep 'iedit) iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'doom-modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(doom-modeline-def-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit))))
    (or (and (not (equal meta "")) meta)
        (if buffer-file-name " %I "))))

;;
;; media-info
;;

(doom-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))

;;
;; bar
;;

(defvar doom-modeline--bar-active nil)
(defvar doom-modeline--bar-inactive nil)
(doom-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (if (doom-modeline--active)
          doom-modeline--bar-active
        doom-modeline--bar-inactive)
    ""))

(when (>= emacs-major-version 26)
  (add-variable-watcher
   'doom-modeline-height
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (doom-modeline-refresh-bars doom-modeline-bar-width val))))

  (add-variable-watcher
   'doom-modeline-bar-width
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (doom-modeline-refresh-bars val doom-modeline-height)))))


;;
;; window number
;;

(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)

(doom-modeline-def-segment window-number
  (if (bound-and-true-p window-numbering-mode)
      (propertize (format " %s " (window-numbering-get-number-string))
                  'face (if (doom-modeline--active)
                            'doom-modeline-bar
                          'doom-modeline-inactive-bar))
    ""))

;;
;; workspace-number
;;

(declare-function eyebrowse--get 'eyebrowse)
(doom-modeline-def-segment workspace-number
  "The current workspace name or number. Requires `eyebrowse-mode' to be
enabled."
  (if (and (bound-and-true-p eyebrowse-mode)
           (< 1 (length (eyebrowse--get 'window-configs))))
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (str (if (and tag (< 0 (length tag)))
                      tag
                    (when num (int-to-string num)))))
        (propertize (format "%s " str) 'face 'doom-modeline-eyebrowse))
    ""))

;;
;; Mode lines
;;

(doom-modeline-def-modeline main
                            (workspace-number window-number bar matches " " buffer-info "  %l:%c %p  " selection-info)
                            (buffer-encoding major-mode vcs flycheck))

(doom-modeline-def-modeline minimal
                            (bar matches " " buffer-info)
                            (media-info major-mode))

(doom-modeline-def-modeline special
                            (bar matches " " buffer-info-simple "  %l:%c %p  " selection-info)
                            (buffer-encoding major-mode flycheck))

(doom-modeline-def-modeline project
                            (bar buffer-default-directory)
                            (major-mode))

(doom-modeline-def-modeline media
                            (bar " %b  ")
                            (media-info major-mode))

;;
;; Hooks
;;

(defun doom-modeline-refresh-bars (&optional width height)
  "Refreash mode-line bars with `WIDTH' and `HEIGHT'."
  (setq doom-modeline--bar-active
        (doom-modeline--make-xpm 'doom-modeline-bar
                                 (or width doom-modeline-bar-width)
                                 (or height doom-modeline-height))
        doom-modeline--bar-inactive
        (doom-modeline--make-xpm 'doom-modeline-inactive-bar
                                 (or width doom-modeline-bar-width)
                                 (or height doom-modeline-height))))

;;;###autoload
(defun doom-modeline-init ()
  "Initialize doom mode-line."
  ;; Create bars
  (doom-modeline-refresh-bars)
  (unless after-init-time
    ;; These buffers are already created and don't get modelines. For the love
    ;; of Emacs, someone give the man a modeline!
    (dolist (bname '("*scratch*" "*Messages*"))
      (with-current-buffer bname
        (doom-modeline-set-modeline 'main)))))

(defun doom-modeline-set-special-modeline ()
  "Set sepcial mode-line."
  (doom-modeline-set-modeline 'special))

(defun doom-modeline-set-media-modeline ()
  "Set media mode-line."
  (doom-modeline-set-modeline 'media))

(defun doom-modeline-set-project-modeline ()
  "Set project mode-line."
  (doom-modeline-set-modeline 'project))

;;
;; Bootstrap
;;

(doom-modeline-set-modeline 'main t) ; set default modeline

;; (add-hook 'doom-load-theme-hook #'doom-modeline-init)
;; (add-hook 'doom-scratch-buffer-hook #'doom-modeline-set-special-modeline)
;; (add-hook 'doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)

(add-hook 'image-mode-hook #'doom-modeline-set-media-modeline)
(add-hook 'circe-mode-hook #'doom-modeline-set-special-modeline)

;; Versions, support Python, Ruby and Golang
(add-hook 'python-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "python --version 2>&1 | cut -d' ' -f2")))
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "ruby --version 2>&1 | cut -d' ' -f2")))
(add-hook 'go-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "go version 2>&1 | cut -d' ' -f3 | tr -d 'go'")))


;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar doom-modeline-remap-face-cookie nil)
(defun doom-modeline-focus ()
  "Focus mode-line."
  (when doom-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative doom-modeline-remap-face-cookie)))
(defun doom-modeline-unfocus ()
  "Unfocus mode-line."
  (setq doom-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(add-hook 'focus-in-hook #'doom-modeline-focus)
(add-hook 'focus-out-hook #'doom-modeline-unfocus)

(provide 'doom-modeline)

;;; doom-modeline.el ends here
