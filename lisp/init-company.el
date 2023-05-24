;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Vincent Zhang

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
;; Auto-completion configurations.
;;

;;; Code:

(require 'init-custom)
(require 'init-funcs)

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind (("M-/"       . company-complete)
         ("C-M-i"     . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("<backtab>" . my-company-yasnippet))
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))
  :config
  (with-no-warnings
    ;; Company anywhere
    ;; @see https://github.com/zk-phi/company-anywhere
    (defun company-anywhere-after-finish (completion)
      (when (and (stringp completion)
                 (looking-at "\\(?:\\sw\\|\\s_\\)+")
                 (save-match-data
                   (string-match (regexp-quote (match-string 0)) completion)))
        (delete-region (match-beginning 0) (match-end 0))))
    (add-hook 'company-after-completion-hook 'company-anywhere-after-finish)

    (defun company-anywhere-grab-word (_)
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
    (advice-add 'company-grab-word :around 'company-anywhere-grab-word)

    (defun company-anywhere-grab-symbol (_)
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))
    (advice-add 'company-grab-symbol :around 'company-anywhere-grab-symbol)

    (defun company-anywhere-dabbrev-prefix (_)
      (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)" company-dabbrev-char-regexp) 1))
    (advice-add 'company-dabbrev--prefix :around 'company-anywhere-dabbrev-prefix)

    (defun company-anywhere-capf (fn command &rest args)
      (if (eq command 'prefix)
          (let ((res (company--capf-data)))
            (when res
              (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
                    (prefix (buffer-substring-no-properties (nth 1 res) (point))))
                (cond
                 (length (cons prefix length))
                 (t prefix)))))
        (apply fn command args)))
    (advice-add 'company-capf :around 'company-anywhere-capf)

    (defun company-anywhere-preview-show-at-point (pos completion)
      (when (and (save-excursion
                   (goto-char pos)
                   (looking-at "\\(?:\\sw\\|\\s_\\)+"))
                 (save-match-data
                   (string-match (regexp-quote (match-string 0)) completion)))
        (move-overlay company-preview-overlay (overlay-start company-preview-overlay) (match-end 0))
        (let ((after-string (overlay-get company-preview-overlay 'after-string)))
          (when after-string
            (overlay-put company-preview-overlay 'display after-string)
            (overlay-put company-preview-overlay 'after-string nil)))))
    (advice-add 'company-preview-show-at-point :after 'company-anywhere-preview-show-at-point)

    ;; `yasnippet' integration
    (with-eval-after-load 'yasnippet
      (defun my-company-yasnippet ()
        "Hide the current completeions and show snippets."
        (interactive)
        (company-cancel)
        (call-interactively 'company-yasnippet))

      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq cmd  'prefix)
            (when-let ((prefix (funcall fn 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fn cmd arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))

  ;; Better sorting
  (use-package prescient
    :commands prescient-persist-mode
    :init (prescient-persist-mode 1))
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Icons and quickhelp
  (use-package company-box
    :diminish
    :bind (:map company-active-map
           ([remap company-show-doc-buffer] . company-box-doc-manually))
    :hook (company-mode . company-box-mode)
    :init (setq company-box-enable-icon centaur-icon
                company-box-backends-colors nil
                company-box-doc-delay 0.1)
    :config
    (with-no-warnings
      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

      ;; Display borders and optimize performance
      (defun my-company-box--display (string on-update)
        "Display the completions."
        (company-box--render-buffer string on-update)

        (let ((frame (company-box--get-frame))
              (border-color (face-foreground 'font-lock-comment-face nil t)))
          (unless frame
            (setq frame (company-box--make-frame))
            (company-box--set-frame frame))
          (company-box--compute-frame-position frame)
          (company-box--move-selection t)
          (company-box--update-frame-position frame)
          (unless (frame-visible-p frame)
            (make-frame-visible frame))
          (company-box--update-scrollbar frame t)
          (set-face-background 'internal-border border-color frame)
          (when (facep 'child-frame-border)
            (set-face-background 'child-frame-border border-color frame)))
        (with-current-buffer (company-box--get-buffer)
          (company-box--maybe-move-number (or company-box--last-start 1))))
      (advice-add #'company-box--display :override #'my-company-box--display)

      (setq company-box-doc-frame-parameters '((vertical-scroll-bars . nil)
                                               (horizontal-scroll-bars . nil)
                                               (internal-border-width . 1)
                                               (left-fringe . 8)
                                               (right-fringe . 8)))

      (defun my-company-box-doc--make-buffer (object)
        (let* ((buffer-list-update-hook nil)
               (inhibit-modification-hooks t)
               (string (cond ((stringp object) object)
                             ((bufferp object) (with-current-buffer object (buffer-string))))))
          (when (and string (length> (string-trim string) 0))
            (with-current-buffer (company-box--get-buffer "doc")
              (erase-buffer)
              (insert (propertize "\n" 'face '(:height 0.5)))
              (insert string)
              (insert (propertize "\n\n" 'face '(:height 0.5)))

              ;; Handle hr lines of markdown
              ;; @see `lsp-ui-doc--handle-hr-lines'
              (let (bolp next before after)
                (goto-char 1)
                (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                  (when (get-text-property next 'markdown-hr)
                    (goto-char next)
                    (setq bolp (bolp)
                          before (char-before))
                    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                    (setq after (char-after (1+ (point))))
                    (insert
                     (concat
                      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                      (propertize "\n" 'face '(:height 0.5))
                      (propertize " "
                                  'display '(space :height (1))
                                  'company-box-doc--replace-hr t
                                  'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
                      (propertize " " 'display '(space :height (1)))
                      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5))))))))

              (setq mode-line-format nil
                    display-line-numbers nil
                    header-line-format nil
                    show-trailing-whitespace nil
                    cursor-in-non-selected-windows nil)
              (current-buffer)))))
      (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

      ;; Display the border and fix the markdown header properties
      (defun my-company-box-doc--show (selection frame)
        (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                  (window-configuration-change-hook nil)
                  (inhibit-redisplay t)
                  (display-buffer-alist nil)
                  (buffer-list-update-hook nil))
          (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                         company-box--bottom
                                         company-selection
                                         (company-box--get-frame)
                                         (frame-visible-p (company-box--get-frame))))
                       (candidate (nth selection company-candidates))
                       (doc (or (company-call-backend 'quickhelp-string candidate)
                                (company-box-doc--fetch-doc-buffer candidate)))
                       (doc (company-box-doc--make-buffer doc)))
            (let ((frame (frame-local-getq company-box-doc-frame))
                  (border-color (face-foreground 'font-lock-comment-face nil t)))
              (unless (frame-live-p frame)
                (setq frame (company-box-doc--make-frame doc))
                (frame-local-setq company-box-doc-frame frame))
              (set-face-background 'internal-border border-color frame)
              (when (facep 'child-frame-border)
                (set-face-background 'child-frame-border border-color frame))
              (company-box-doc--set-frame-position frame)

              ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
              (with-current-buffer (company-box--get-buffer "doc")
                (let (next)
                  (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                    (when (get-text-property next 'company-box-doc--replace-hr)
                      (put-text-property next (1+ next) 'display
                                         '(space :align-to (- right-fringe 1) :height (1)))
                      (put-text-property (1+ next) (+ next 2) 'display
                                         '(space :align-to right-fringe :height (1)))))))

              (unless (frame-visible-p frame)
                (make-frame-visible frame))))))
      (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

      (defun my-company-box-doc--set-frame-position (frame)
        (-let* ((frame-resize-pixelwise t)

                (box-frame (company-box--get-frame))
                (box-position (frame-position box-frame))
                (box-width (frame-pixel-width box-frame))
                (box-height (frame-pixel-height box-frame))
                ;; (box-border-width (frame-border-width box-frame))

                (window (frame-root-window frame))
                ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                    (/ (frame-pixel-width) 2)
                                                                    (/ (frame-pixel-height) 2)))
                (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

                (x (- (+ (car box-position) box-width) border-width))
                (space-right (- (frame-pixel-width) x))
                (space-left (car box-position))
                (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
                (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
                (width (+ text-width border-width fringe-left fringe-right))
                (x (if (> width space-right)
                       (if (> space-left width)
                           (- space-left width)
                         space-left)
                     x))
                (y (cdr box-position))
                (bottom (+ company-box--bottom (frame-border-width)))
                (height (+ text-height (* 2 border-width)))
                (y (cond ((= x space-left)
                          (if (> (+ y box-height height) bottom)
                              (+ (- y height) border-width)
                            (- (+ y box-height) border-width)))
                         ((> (+ y height) bottom)
                          (- (+ y box-height) height))
                         (t y))))
          (set-frame-position frame (max x 0) (max y 0))
          (set-frame-size frame text-width text-height t)))
      (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

      (when (icons-displayable-p)
        (defvar company-box-icons-nerd
          `((Unknown       . ,(nerd-icons-codicon "nf-cod-symbol_namespace"))
            (Text          . ,(nerd-icons-codicon "nf-cod-symbol_string"))
            (Method        . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
            (Function      . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
            (Constructor   . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-lpurple))
            (Field         . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'nerd-icons-lblue))
            (Variable      . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'nerd-icons-lblue))
            (Class         . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-orange))
            (Interface     . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'nerd-icons-lblue))
            (Module        . ,(nerd-icons-codicon "nf-cod-symbol_namespace" :face 'nerd-icons-lblue))
            (Property      . ,(nerd-icons-codicon "nf-cod-symbol_property"))
            (Unit          . ,(nerd-icons-codicon "nf-cod-symbol_key"))
            (Value         . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'nerd-icons-lblue))
            (Enum          . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'nerd-icons-orange))
            (Keyword       . ,(nerd-icons-codicon "nf-cod-symbol_keyword"))
            (Snippet       . ,(nerd-icons-codicon "nf-cod-symbol_snippet"))
            (Color         . ,(nerd-icons-codicon "nf-cod-symbol_color"))
            (File          . ,(nerd-icons-codicon "nf-cod-symbol_file"))
            (Reference     . ,(nerd-icons-codicon "nf-cod-symbol_misc"))
            (Folder        . ,(nerd-icons-codicon "nf-cod-folder"))
            (EnumMember    . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'nerd-icons-lblue))
            (Constant      . ,(nerd-icons-codicon "nf-cod-symbol_constant"))
            (Struct        . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'nerd-icons-orange))
            (Event         . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'nerd-icons-orange))
            (Operator      . ,(nerd-icons-codicon "nf-cod-symbol_operator"))
            (TypeParameter . ,(nerd-icons-codicon "nf-cod-symbol_class"))
            (Template      . ,(nerd-icons-codicon "nf-cod-symbol_snippet"))))
        (setq company-box-icons-alist 'company-box-icons-nerd))
      )))

(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
