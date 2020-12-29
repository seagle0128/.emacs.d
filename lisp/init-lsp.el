;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Vincent Zhang

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
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(pcase centaur-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish
     :defines (lsp-clients-python-library-directories
               lsp-rust-server)
     :commands (lsp-enable-which-key-integration
                lsp-format-buffer
                lsp-organize-imports
                lsp-install-server)
     :custom-face
     (lsp-headerline-breadcrumb-path-error-face
      ((t :underline (:style line :color ,(face-foreground 'error))
          :inherit lsp-headerline-breadcrumb-path-face)))
     (lsp-headerline-breadcrumb-path-warning-face
      ((t :underline (:style line :color ,(face-foreground 'warning))
          :inherit lsp-headerline-breadcrumb-path-face)))
     (lsp-headerline-breadcrumb-path-info-face
      ((t :underline (:style line :color ,(face-foreground 'success))
          :inherit lsp-headerline-breadcrumb-path-face)))
     (lsp-headerline-breadcrumb-path-hint-face
      ((t :underline (:style line :color ,(face-foreground 'success))
          :inherit lsp-headerline-breadcrumb-path-face)))

     (lsp-headerline-breadcrumb-symbols-error-face
      ((t :inherit lsp-headerline-breadcrumb-symbols-face
          :underline (:style line :color ,(face-foreground 'error)))))
     (lsp-headerline-breadcrumb-symbols-warning-face
      ((t :inherit lsp-headerline-breadcrumb-symbols-face
          :underline (:style line :color ,(face-foreground 'warning)))))
     (lsp-headerline-breadcrumb-symbols-info-face
      ((t :inherit lsp-headerline-breadcrumb-symbols-face
          :underline (:style line :color ,(face-foreground 'success)))))
     (lsp-headerline-breadcrumb-symbols-hint-face
      ((t :inherit lsp-headerline-breadcrumb-symbols-face
          :underline (:style line :color ,(face-foreground 'success)))))

     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                             (lsp-deferred))))
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          ;; Format and organize imports
                          (unless (apply #'derived-mode-p centaur-lsp-format-on-save-ignore-modes)
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
     :init
     ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
     (setq read-process-output-max (* 1024 1024)) ;; 1MB

     (setq lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-signature-auto-activate nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil
           lsp-modeline-workspace-status-enable nil

           lsp-enable-file-watchers nil
           lsp-enable-folding nil
           lsp-enable-symbol-highlighting nil
           lsp-enable-text-document-color nil

           lsp-enable-indentation nil
           lsp-enable-on-type-formatting nil)

     ;; For `lsp-clients'
     (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     (when (executable-find "rust-analyzer")
       (setq lsp-rust-server 'rust-analyzer))
     :config
     (with-no-warnings
       (defun my-lsp--init-if-visible (func &rest args)
         "Not enabling lsp in `git-timemachine-mode'."
         (unless (bound-and-true-p git-timemachine-mode)
           (apply func args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible))

     (defun lsp-update-server ()
       "Update LSP server."
       (interactive)
       ;; Equals to `C-u M-x lsp-install-server'
       (lsp-install-server t)))

   (use-package lsp-ui
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :pretty-hydra
     ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
       :color amaranth :quit-key "q")
      ("Doc"
       (("d e" (progn
                 (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                 (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
         "enable" :toggle lsp-ui-doc-mode)
        ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
         "signature" :toggle lsp-ui-doc-include-signature)
        ("d t" (setq lsp-ui-doc-position 'top)
         "top" :toggle (eq lsp-ui-doc-position 'top))
        ("d b" (setq lsp-ui-doc-position 'bottom)
         "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
        ("d p" (setq lsp-ui-doc-position 'at-point)
         "at point" :toggle (eq lsp-ui-doc-position 'at-point))
        ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
         "header" :toggle lsp-ui-doc-header)
        ("d f" (setq lsp-ui-doc-alignment 'frame)
         "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
        ("d w" (setq lsp-ui-doc-alignment 'window)
         "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
       "Sideline"
       (("s e" (progn
                 (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
                 (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
         "enable" :toggle lsp-ui-sideline-mode)
        ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
         "hover" :toggle lsp-ui-sideline-show-hover)
        ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
         "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
        ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
         "symbol" :toggle lsp-ui-sideline-show-symbol)
        ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
         "code actions" :toggle lsp-ui-sideline-show-code-actions)
        ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
         "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
       "Action"
       (("h" backward-char "←")
        ("j" next-line "↓")
        ("k" previous-line "↑")
        ("l" forward-char "→")
        ("C-a" mwim-beginning-of-code-or-line nil)
        ("C-e" mwim-end-of-code-or-line nil)
        ("C-b" backward-char nil)
        ("C-n" next-line nil)
        ("C-p" previous-line nil)
        ("C-f" forward-char nil)
        ("M-b" backward-word nil)
        ("M-f" forward-word nil)
        ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
     :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body)
            ("M-RET" . lsp-ui-sideline-apply-code-actions))
     :hook (lsp-mode . lsp-ui-mode)
     :init (setq lsp-ui-sideline-show-diagnostics nil
                 lsp-ui-sideline-ignore-duplicate t
                 lsp-ui-doc-position 'at-point
                 lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
                 lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                       ,(face-foreground 'font-lock-string-face)
                                       ,(face-foreground 'font-lock-constant-face)
                                       ,(face-foreground 'font-lock-variable-name-face)))
     :config
     ;; `C-g'to close doc
     (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

     ;; Reset `lsp-ui-doc-background' after loading theme
     (add-hook 'after-load-theme-hook
               (lambda ()
                 (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
                 (set-face-background 'lsp-ui-doc-background (face-background 'tooltip)))))

   ;; Ivy integration
   (use-package lsp-ivy
     :after lsp-mode
     :bind (:map lsp-mode-map
            ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
            ("C-s-." . lsp-ivy-global-workspace-symbol)))

   ;; Debug
   (when emacs/>=26p
     (use-package dap-mode
       :defines dap-python-executable
       :functions dap-hydra/nil
       :diminish
       :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
       :hook ((after-init . dap-auto-configure-mode)
              (dap-stopped . (lambda (_args) (dap-hydra)))
              (dap-terminated . (lambda (_args) (dap-hydra/nil)))

              (python-mode . (lambda () (require 'dap-python)))
              (ruby-mode . (lambda () (require 'dap-ruby)))
              (go-mode . (lambda () (require 'dap-go)))
              (java-mode . (lambda () (require 'dap-java)))
              ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
              (php-mode . (lambda () (require 'dap-php)))
              (elixir-mode . (lambda () (require 'dap-elixir)))
              ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
              (powershell-mode . (lambda () (require 'dap-pwsh))))
       :init
       (when (executable-find "python3")
         (setq dap-python-executable "python3"))))

   ;; `lsp-mode' and `treemacs' integration
   (when emacs/>=25.2p
     (use-package lsp-treemacs
       :after lsp-mode
       :bind (:map lsp-mode-map
              ("C-<f8>" . lsp-treemacs-errors-list)
              ("M-<f8>" . lsp-treemacs-symbols)
              ("s-<f8>" . lsp-treemacs-java-deps-list))
       :init (lsp-treemacs-sync-mode 1)
       :config
       (with-eval-after-load 'ace-window
         (when (boundp 'aw-ignored-buffers)
           (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
           (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers)))

       (with-no-warnings
         (when (require 'all-the-icons nil t)
           (treemacs-create-theme "centaur-colors"
             :extends "doom-colors"
             :config
             (progn
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
                :extensions (root))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                :extensions (boolean-data))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
                :extensions (class))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
                :extensions (color-palette))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
                :extensions (constant))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
                :extensions (document))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
                :extensions (enumerator))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                :extensions (enumitem))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
                :extensions (event))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                :extensions (field))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
                :extensions (indexer))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
                :extensions (intellisense-keyword))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                :extensions (interface))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                :extensions (localvariable))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
                :extensions (method))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                :extensions (namespace))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
                :extensions (numeric))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
                :extensions (operator))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                :extensions (property))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
                :extensions (snippet))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
                :extensions (string))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
                :extensions (structure))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
                :extensions (template))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                :extensions (collapsed) :fallback "+")
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                :extensions (expanded) :fallback "-")
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
                :extensions (classfile))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
                :extensions (default-folder-opened))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
                :extensions (default-folder))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
                :extensions (default-root-folder-opened))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
                :extensions (default-root-folder))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                :extensions ("class"))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                :extensions (file-type-jar))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (folder-open))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                :extensions (folder))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
                :extensions (folder-type-component-opened))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
                :extensions (folder-type-component))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
                :extensions (folder-type-library-opened))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
                :extensions (folder-type-library))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
                :extensions (folder-type-maven-opened))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
                :extensions (folder-type-maven))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
                :extensions (folder-type-package-opened))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
                :extensions (folder-type-package))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (icon-create))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (icon-flat))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
                :extensions (icon-hierarchical))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (icon-link))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (icon-refresh))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (icon-unlink))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
                :extensions (jar))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
                :extensions (library))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
                :extensions (packagefolder-open))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                :extensions (packagefolder))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                :extensions (package))
               (treemacs-create-icon
                :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
                :extensions (java-project))))

           (setq lsp-treemacs-theme "centaur-colors")))))

   ;; Python: pyright
   (use-package lsp-pyright
     :preface
     ;; Use yapf to format
     (defun lsp-pyright-format-buffer ()
       (interactive)
       (when (and (executable-find "yapf") buffer-file-name)
         (call-process "yapf" nil nil nil "-i" buffer-file-name)))
     :hook (python-mode . (lambda ()
                            (require 'lsp-pyright)
                            (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
     :init (when (executable-find "python3")
             (setq lsp-pyright-python-executable-cmd "python3")))

   ;; C/C++/Objective-C support
   (use-package ccls
     :defines projectile-project-root-files-top-down-recurring
     :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
     :config
     (with-eval-after-load 'projectile
       (setq projectile-project-root-files-top-down-recurring
             (append '("compile_commands.json" ".ccls")
                     projectile-project-root-files-top-down-recurring))))

   ;; Swift/C/C++/Objective-C
   (when sys/macp
     (use-package lsp-sourcekit
       :init (setq lsp-sourcekit-executable
                   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))

   ;; Julia support
   (use-package lsp-julia
     :hook (julia-mode . (lambda () (require 'lsp-julia))))

   ;; Java support
   (when emacs/>=25.2p
     (use-package lsp-java
       :hook (java-mode . (lambda () (require 'lsp-java)))))))

(when centaur-lsp
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (user-error "LSP:: specify `:file' property to enable"))

             (setq buffer-file-name file-name)
             (pcase centaur-lsp
               ('eglot
                (and (fboundp 'eglot-ensure) (eglot-ensure)))
               ('lsp-mode
                (and (fboundp 'lsp-deferred) (lsp-deferred)))
               (_ (user-error "LSP:: invalid `centaur-lsp' type")))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      centaur-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
