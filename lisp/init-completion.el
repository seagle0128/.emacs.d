;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2016-2025 Vincent Zhang

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
;; Modern completion configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Support Pinyin
(use-package pinyinlib
  :after orderless
  :functions orderless-regexp
  :autoload pinyinlib-build-regexp-string
  :init
  (defun orderless-regexp-pinyin (str)
    "Match COMPONENT as a pinyin regex."
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin))

;; VERTical Interactive COmpletion
(use-package vertico
  :custom (vertico-count 15)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Display vertico in the child frame
(use-package vertico-posframe
  :functions posframe-poshandler-frame-center-near-bottom
  :hook (vertico-mode . vertico-posframe-mode)
  :init (setq vertico-posframe-poshandler
              #'posframe-poshandler-frame-center-near-bottom
              vertico-posframe-parameters
              '((left-fringe  . 8)
                (right-fringe . 8))))

;; Add icons to completion candidates
(use-package nerd-icons-completion
  :hook (vertico-mode . nerd-icons-completion-mode))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Consulting completing-read
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ("C-c r"   . consult-ripgrep)
         ("C-c T"   . consult-theme)
         ("C-."     . consult-imenu)

         ("C-c c e" . consult-colors-emacs)
         ("C-c c w" . consult-colors-web)
         ("C-c c f" . describe-face)
         ("C-c c l" . find-library)
         ("C-c c t" . consult-theme)

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)            ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)             ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)      ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)      ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                 ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)           ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                  ;; orig. next-matching-history-element
         ("M-r" . consult-history))                 ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  ;; More utils
  (defvar consult-colors-history nil
    "History for `consult-colors-emacs' and `consult-colors-web'.")

  ;; No longer preloaded in Emacs 28.
  (autoload 'list-colors-duplicates "facemenu")
  ;; No preloaded in consult.el
  (autoload 'consult--read "consult")

  (defun consult-colors-emacs (color)
    "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (list-colors-duplicates (defined-colors))
                          :prompt "Emacs color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))

  ;; Adapted from counsel.el to get web colors.
  (defun consult-colors--web-list nil
    "Return list of CSS colors for `counsult-colors-web'."
    (require 'shr-color)
    (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

  (defun consult-colors-web (color)
    "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (consult-colors--web-list)
                          :prompt "Color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  (setq consult-preview-key nil)
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package consult-flyspell
  :bind ("M-g s" . consult-flyspell))

(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet))

(use-package embark
  :bind (("s-."   . embark-act)
         ("C-s-." . embark-act)
         ("M-."   . embark-dwim)        ; overrides `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("M-." . my-embark-preview))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Manual preview for non-Consult commands using Embark
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (with-no-warnings
    (with-eval-after-load 'which-key
      (defun embark-which-key-indicator ()
       "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
       (lambda (&optional keymap targets prefix)
         (if (null keymap)
             (which-key--hide-popup-ignore-command)
           (which-key--show-keymap
            (if (eq (plist-get (car targets) :type) 'embark-become)
                "Become"
              (format "Act on %s '%s'%s"
                      (plist-get (car targets) :type)
                      (embark--truncate-target (plist-get (car targets) :target))
                      (if (cdr targets) "…" "")))
            (if prefix
                (pcase (lookup-key keymap prefix 'accept-default)
                  ((and (pred keymapp) km) km)
                  (_ (key-binding prefix 'accept-default)))
              keymap)
            nil nil t (lambda (binding)
                        (not (string-suffix-p "-argument" (cdr binding))))))))

      (setq embark-indicators
           '(embark-which-key-indicator
             embark-highlight-indicator
             embark-isearch-highlight-indicator))

      (defun embark-hide-which-key-indicator (fn &rest args)
       "Hide the which-key indicator immediately when using the completing-read prompter."
       (which-key--hide-popup-ignore-command)
       (let ((embark-indicators
              (remq #'embark-which-key-indicator embark-indicators)))
         (apply fn args)))

      (advice-add #'embark-completing-read-prompter
                 :around #'embark-hide-which-key-indicator))))

(use-package embark-consult
  :bind (:map minibuffer-mode-map
         ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Auto completion
(use-package corfu
  :autoload consult-completion-in-region
  :functions corfu-move-to-minibuffer
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-count 12)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (global-corfu-modes '((not erc-mode
                             circe-mode
                             help-mode
                             gud-mode
                             vterm-mode)
                        t))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         (global-corfu-mode . corfu-history-mode))
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

  ;; Make these capfs composable.
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; Sanitize the `pcomplete-completions-at-point' Capf.  The Capf has undesired
  ;; side effects on Emacs 28.  These advices are not needed on Emacs 29 and newer.
  (unless emacs/>=29p
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
