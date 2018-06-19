;;; doom-modeline.el  -*- lexical-binding: t; -*-

(use-package dash
  :commands (-when-let*))

(use-package projectile
  :commands (projectile-project-root projectile-require-project-root))

(when (and (display-graphic-p) (not (eq system-type 'windows-nt)))
  (use-package all-the-icons
    :preface (defvar all-the-icons-default-adjust -0.2)
    :commands (all-the-icons-alltheicon
               all-the-icons-octicon
               all-the-icons-faicon
               all-the-icons-material)))

(eval-and-compile
  (defun doom--resolve-hooks (hooks)
    (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
             for hook in (doom-enlist (doom-unquote hooks))
             if (eq (car-safe hook) 'quote)
             collect (cadr hook)
             else if quoted-p
             collect hook
             else collect (intern (format "%s-hook" (symbol-name hook)))))

  (defun doom-enlist (exp)
    "Return EXP wrapped in a list, or as-is if already a list."
    (if (listp exp) exp (list exp)))

  (defun doom-unquote (exp)
    "Return EXP unquoted."
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp)

  (defvar doom--transient-counter 0))

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient forms to a HOOK.
HOOK can be a quoted hook or a sharp-quoted function (which will be advised).
These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append (eq (car forms) :after))
        (fn (intern (format "doom-transient-hook-%s" (cl-incf doom--transient-counter)))))
    `(when ,hook
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                     ((symbolp ,hook)   (remove-hook ,hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook)
              (advice-add ,hook ,(if append :after :before) #',fn))
             ((symbolp ,hook)
              (add-hook ,hook #',fn ,append))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:
  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.
Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))
Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hooks (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (cond ((eq hook-fn 'remove-hook)
                       `(remove-hook ',hook ,fn ,local-p))
                      (t
                       `(add-hook ',hook ,fn ,append-p ,local-p)))
                forms)))
      `(progn ,@(nreverse forms)))))

(defmacro def-modeline-segment! (name &rest forms)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst doom--prepare-modeline-segments (segments)
  (cl-loop for seg in segments
           if (stringp seg)
           collect seg
           else
           collect (list (intern (format "doom-modeline-segment--%s" (symbol-name seg))))))

(defmacro def-modeline! (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it. NAME is a symbol to identify
it (used by `doom-modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `def-modeline-segment!'.
Example:
  (def-modeline! minimal
    (bar matches \" \" buffer-info)
    (media-info major-mode))
  (doom-set-modeline 'minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom--prepare-modeline-segments lhs))
        (rhs-forms (doom--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun doom-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun doom-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (-when-let* ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))

(defun doom-project-root ()
  "Get the path to the root of your project.
If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))

;;
;; modeline configs
;;

(use-package eldoc-eval
  :hook (after-init . eldoc-in-minibuffer-mode)
  :config
  (defun +doom-modeline-eldoc (text)
    (concat (when (display-graphic-p)
              (+doom-modeline--make-xpm
               (face-background 'doom-modeline-eldoc-bar nil t)
               +doom-modeline-height
               +doom-modeline-bar-width))
            text))

  ;; Show eldoc in the mode-line with `eval-expression'
  (defun +doom-modeline--show-eldoc (input)
    "Display string STR in the mode-line next to minibuffer."
    (with-current-buffer (eldoc-current-buffer)
      (let* ((str              (and (stringp input) input))
             (mode-line-format (or (and str (or (+doom-modeline-eldoc str) str))
                                   mode-line-format))
             mode-line-in-non-selected-windows)
        (force-mode-line-update)
        (sit-for eldoc-show-in-mode-line-delay))))
  (setq eldoc-in-minibuffer-show-fn #'+doom-modeline--show-eldoc))

;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
(when (featurep 'evil)
  (use-package evil-anzu
    :defer t
    :init
    (add-transient-hook! #'evil-ex-start-search (require 'evil-anzu))
    :config
    (setq anzu-cons-mode-line-p nil
          anzu-minimum-input-length 1
          anzu-search-threshold 250)

    ;; Avoid anzu conflicts across buffers
    (mapc #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
                                anzu--cached-count anzu--cached-positions anzu--last-command
                                anzu--last-isearch-string anzu--overflow-p))

    ;; Ensure anzu state is cleared when searches & iedit are done
    (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
    (add-hook '+evil-esc-hook #'anzu--reset-status t)
    (add-hook 'iedit-mode-end-hook #'anzu--reset-status)))


;; Keep `+doom-modeline-current-window' up-to-date
(defvar +doom-modeline-current-window (frame-selected-window))
(defun +doom-modeline|set-selected-window (&rest _)
  "Sets `+doom-modeline-current-window' appropriately"
  (-when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +doom-modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'+doom-modeline|set-selected-window)
(add-hook 'focus-in-hook #'+doom-modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'+doom-modeline|set-selected-window)
(advice-add #'select-window :after #'+doom-modeline|set-selected-window)

;; fish-style modeline
(use-package shrink-path
  :commands (shrink-path-prompt shrink-path-file-mixed))


;;
;; Variables
;;

(defvar +doom-modeline-height 29
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +doom-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defvar +doom-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `+doom-modeline-buffer-file-name'.
Given ~/Projects/FOSS/emacs/lisp/comint.el
truncate-upto-project => ~/P/F/emacs/lisp/comint.el
truncate-upto-root => ~/P/F/e/lisp/comint.el
truncate-all => ~/P/F/e/l/comint.el
relative-from-project => emacs/lisp/comint.el
relative-to-project => lisp/comint.el
file-name => comint.el")

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)


;;
;; Custom faces
;;

(defgroup +doom-modeline nil
  ""
  :group 'doom)

(defface doom-modeline-buffer-path
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the dirname part of the buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-file
  '((t (:inherit mode-line-buffer-id)))
  "Face used for the filename part of the mode-line buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-modified
  '((t (:inherit error :background nil :bold t)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the major-mode segment in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+doom-modeline--anzu', `+doom-modeline--evil-substitute' and
`iedit'"
  :group '+doom-modeline)

(defface doom-modeline-info
  `((t (:inherit success :bold t)))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+doom-modeline)

(defface doom-modeline-warning
  `((t (:inherit warning :bold t)))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defface doom-modeline-urgent
  `((t (:inherit error :bold t)))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+doom-modeline)

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group '+doom-modeline)

(defface doom-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+doom-modeline)

(defface doom-modeline-eyebrowse '((t ()))
  "The face used for eyebrowse."
  :group '+doom-modeline)

(defface doom-modeline-bracket '((t (:inherit shadow)))
  "The face used for brackets around the project."
  :group '+doom-modeline)

;;
;; Bootstrap
;;

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local +doom-modeline-env-version nil)
(defvar-local +doom-modeline-env-command nil)
(add-hook! '(focus-in-hook find-file-hook) #'+doom-modeline|update-env)
(defun +doom-modeline|update-env ()
  (when +doom-modeline-env-command
    (let* ((default-directory (doom-project-root))
           (s (shell-command-to-string +doom-modeline-env-command)))
      (setq +doom-modeline-env-version (if (string-match "[ \t\n\r]+\\'" s)
                                           (replace-match "" t t s)
                                         s)))))

;; Only support python and ruby for now

;; TODO torgeir
(add-hook! 'python-mode-hook (setq +doom-modeline-env-command "python --version 2>&1 | cut -d' ' -f2"))
(add-hook! 'ruby-mode-hook   (setq +doom-modeline-env-command "ruby   --version 2>&1 | cut -d' ' -f2"))

;;
;; Modeline helpers
;;

(defsubst active ()
  (eq (selected-window) +doom-modeline-current-window))

;; Inspired from `powerline's `pl/make-xpm'.
(defun +doom-modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
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
      'xpm t :ascent 'center))))

(defun +doom-modeline-buffer-file-name ()
  "Propertized `buffer-file-name' based on `+doom-modeline-buffer-file-name-style'."
  (propertize
   (pcase +doom-modeline-buffer-file-name-style
     (`truncate-upto-project (+doom-modeline--buffer-file-name 'shrink))
     (`truncate-upto-root (+doom-modeline--buffer-file-name-truncate))
     (`truncate-all (+doom-modeline--buffer-file-name-truncate t))
     (`relative-to-project (+doom-modeline--buffer-file-name-relative))
     (`relative-from-project (+doom-modeline--buffer-file-name-relative 'include-project))
     (`file-name (propertize (file-name-nondirectory buffer-file-name)
                             'face
                             (let ((face (or (and (buffer-modified-p)
                                                  'doom-modeline-buffer-modified)
                                             (and (active)
                                                  'doom-modeline-buffer-file))))
                               (when face `(:inherit ,face))))))
   'help-echo buffer-file-truename))

(defun +doom-modeline--buffer-file-name-truncate (&optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename)))
        (active (active)))
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

(defun +doom-modeline--buffer-file-name-relative (&optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (doom-project-root))
        (active (active)))
    (if (null root)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory buffer-file-truename)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory buffer-file-truename)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun +doom-modeline--buffer-file-name (truncate-project-root-parent)
  "Propertized `buffer-file-name'.
If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
fish-shell style.
Example:
~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (doom-project-root))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-truename)
                                                  buffer-file-truename))
         (active (active)))
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
;; Segments
;;


(def-modeline-segment! buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (active) 'doom-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (+doom-maybe-icon-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))


;;
(def-modeline-segment! buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat
   (cond (buffer-read-only
          (concat (+doom-maybe-icon-octicon
                   "lock"
                   :face 'doom-modeline-warning
                   :v-adjust -0.05)
                  " "))
         ((buffer-modified-p)
          (concat (+doom-maybe-icon-faicon
                   "floppy-o"
                   :face 'doom-modeline-buffer-modified
                   :v-adjust -0.0575)
                  " "))
         ((and buffer-file-name
               (not (file-exists-p buffer-file-name)))
          (concat (+doom-maybe-icon-octicon
                   "circle-slash"
                   :face 'doom-modeline-urgent
                   :v-adjust -0.05)
                  " "))
         ((buffer-narrowed-p)
          (concat (+doom-maybe-icon-octicon
                   "fold"
                   :face 'doom-modeline-warning
                   :v-adjust -0.05)
                  " ")))
   (if buffer-file-name
       (+doom-modeline-buffer-file-name)
     "%b")))

;;
(def-modeline-segment! buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'doom-modeline-buffer-modified)
               ((active) 'doom-modeline-buffer-file))))

;;
(def-modeline-segment! buffer-encoding
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
(def-modeline-segment! major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (when +doom-modeline-env-version
             (concat " " +doom-modeline-env-version))
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'doom-modeline-buffer-major-mode)))


(defun +doom-maybe-icon-octicon (&rest args)
  (when (and (display-graphic-p) (not (eq system-type 'windows-nt)))
    (apply 'all-the-icons-octicon args)))

(defun +doom-maybe-icon-faicon (&rest args)
  (when (and (display-graphic-p) (not (eq system-type 'windows-nt)))
    (apply 'all-the-icons-faicon args)))

(defun +doom-maybe-icon-material (&rest args)
  (when (and (display-graphic-p) (not (eq system-type 'windows-nt)))
    (apply 'all-the-icons-material args)))

;;
(def-modeline-segment! vcs
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (concat (if (display-graphic-p) "  ")
                (cond ((memq state '(edited added))
                       (if active (setq face 'doom-modeline-info))
                       (+doom-maybe-icon-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face 'doom-modeline-info))
                       (+doom-maybe-icon-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face 'doom-modeline-warning))
                       (+doom-maybe-icon-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face 'doom-modeline-urgent))
                       (+doom-maybe-icon-octicon "alert" :face face))
                      (t
                       (if active (setq face 'font-lock-doc-face))
                       (+doom-maybe-icon-octicon
                        "git-branch"
                        :face face
                        :v-adjust -0.05)))
                " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))


;;
(defun +doom-ml-icon (icon &optional text face voffset)
  "Displays an ICON with FACE, followed by TEXT. Uses
`all-the-icons-material' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (+doom-maybe-icon-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +doom-modeline-vspc)))
          (when text
            (propertize text 'face face))
          (if vc-mode "  " " ")))

(def-modeline-segment! flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      (`finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (+doom-ml-icon "do_not_disturb_alt"
                                        (number-to-string sum)
                                        (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                        -0.25)))
                   (+doom-ml-icon "check" nil 'doom-modeline-info)))
      (`running     (+doom-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
      (`no-checker  (+doom-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
      (`errored     (+doom-ml-icon "sim_card_alert" "Error" 'doom-modeline-urgent))
      (`interrupted (+doom-ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))
;; ('interrupted (+doom-ml-icon "x" "Interrupted" 'font-lock-doc-face)))))


;;
(defsubst doom-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(def-modeline-segment! selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (active) (or mark-active (eq evil-state 'visual)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (eq 'block evil-visual-selection))
                (let ((cols (abs (- (doom-column reg-end)
                                    (doom-column reg-beg)))))
                  (format "%dx%dB" lines cols)))
               ((eq 'line evil-visual-selection)
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL" (- (1+ reg-end) reg-beg) lines))
               (t
                (format "%dC" (- (1+ reg-end) reg-beg)))))
       'face 'doom-modeline-highlight))))


;;
(defun +doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-panel)
              sep
              (+doom-maybe-icon-octicon "triangle-right"
                                        :face 'doom-modeline-panel
                                        :v-adjust -0.05)
              sep))))

(defsubst +doom-modeline--anzu ()
  "Show the match index and total number thereof. Requires `anzu', also
`evil-anzu' if using `evil-mode' for compatibility with `evil-search'."
  (setq anzu-cons-mode-line-p nil)
  (when (and anzu--state (not iedit-mode))
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
     'face (if (active) 'doom-modeline-panel))))

(defsubst +doom-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and evil-mode
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
     'face (if (active) 'doom-modeline-panel))))

(defun doom-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
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
                                                  #'doom-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) 'doom-modeline-panel))))

(def-modeline-segment! matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+doom-modeline--macro-recording)
                      (+doom-modeline--anzu)
                      (+doom-modeline--evil-substitute)
                      (+doom-modeline--iedit))))
    (or (and (not (equal meta "")) meta)
        (if buffer-file-name " %I "))))

;; TODO Include other information
(def-modeline-segment! media-info
  "Metadata regarding the current file, such as dimensions for images."
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))

(def-modeline-segment! bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (+doom-modeline--make-xpm
       (face-background (if (active)
                            'doom-modeline-bar
                          'doom-modeline-inactive-bar)
                        nil t)
       +doom-modeline-height
       +doom-modeline-bar-width)
    ""))

;;
(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)

(def-modeline-segment! window-number
  (if (bound-and-true-p window-numbering-mode)
      (propertize (format " %s " (window-numbering-get-number-string))
                  'face (if (active)
                            'doom-modeline-bar
                          'doom-modeline-inactive-bar))
    ""))

(declare-function eyebrowse--get 'eyebrowse)
(def-modeline-segment! workspace-number
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

(def-modeline! main
  (workspace-number bar matches " " buffer-info "  %l:%c %p  " selection-info)
  (buffer-encoding major-mode vcs flycheck))

(def-modeline! minimal
  (bar matches " " buffer-info)
  (media-info major-mode))

(def-modeline! special
  (bar matches " " buffer-info-simple "  %l:%c %p  " selection-info)
  (buffer-encoding major-mode flycheck))

(def-modeline! project
  (bar buffer-default-directory)
  (major-mode))

(def-modeline! media
  (bar " %b  ")
  (media-info major-mode))

;;
;; Bootstrap
;;

(defun +doom-modeline|init ()
  "Set the default modeline."
  (doom-set-modeline 'main t)

  ;; This scratch and messages buffer is already created and doesn't get a
  ;; modeline.
  (with-current-buffer "*Messages*"
    (doom-set-modeline 'main))
  (with-current-buffer "*scratch*"
    (doom-set-modeline 'main)))

(defun +doom-modeline|set-special-modeline ()
  "Set the special modeline."
  (doom-set-modeline 'special))

(defun +doom-modeline|set-media-modeline ()
  "Set the media modeline."
  (doom-set-modeline 'media))

(add-hook 'image-mode-hook   #'+doom-modeline|set-media-modeline)
(add-hook 'org-src-mode-hook #'+doom-modeline|set-special-modeline)

(provide 'doom-modeline)

;;; doom-modeline.el ends here
