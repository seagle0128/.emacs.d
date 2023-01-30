;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Vincent Zhang

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
;; Ivy configurations.
;;

;;; Code:

(require 'init-funcs)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:foreground "dimgray" :distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-2 ((t (:distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-3 ((t (:distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-4 ((t (:distant-foreground unspecified :background unspecified))))
  :bind (("C-s"   . swiper-isearch)
         ("C-r"   . swiper-isearch-backward)
         ("s-f"   . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper]             . counsel-grep-or-swiper)
         ([remap swiper-backward]    . counsel-grep-or-swiper-backward)
         ([remap dired]              . counsel-dired)
         ([remap set-variable]       . counsel-set-variable)
         ([remap insert-char]        . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap org-capture]        . counsel-org-capture)

         ("C-x j" . counsel-mark-ring)
         ("C-h F" . counsel-faces)

         ("C-c B" . counsel-bookmarked-directory)
         ("C-c L" . counsel-load-library)
         ("C-c O" . counsel-find-file-extern)
         ("C-c P" . counsel-package)
         ("C-c R" . counsel-list-processes)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-git-log)
         ("C-c o" . counsel-outline)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c B" . counsel-bookmarked-directory)
         ("C-c c F" . counsel-faces)
         ("C-c c L" . counsel-load-library)
         ("C-c c K" . counsel-ace-link)
         ("C-c c O" . counsel-find-file-extern)
         ("C-c c P" . counsel-package)
         ("C-c c R" . counsel-list-processes)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-git-log)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c v" . counsel-set-variable)
         ("C-c c z" . counsel-fzf)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace)

         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-height 12
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
        ivy-on-del-error-function #'ignore
        ivy-initial-inputs-alist nil)

  ;; Use orderless regex strategy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;; Set minibuffer height for different commands
  (setq ivy-height-alist '((counsel-evil-registers . 5)
                           (counsel-yank-pop       . 8)
                           (counsel-git-log        . 4)
                           (swiper                 . 15)
                           (counsel-projectile-ag  . 15)
                           (counsel-projectile-rg  . 15)))

  ;; Better performance on Windows
  (when sys/win32p
    (setq ivy-dynamic-exhibit-delay-ms 200))

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-preselect-current-file t
        counsel-yank-pop-separator "\n────────\n")
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Use the faster search tools
  (cond
   ((executable-find "ugrep")
    (setq counsel-grep-base-command "ugrep --color=never -n -e '%s' '%s'"))
   ((executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'")))

  (when (executable-find "fd")
    (setq counsel-fzf-cmd
          "fd --type f --hidden --follow --exclude .git --color never '%s'"))

  ;; Be compatible with `gls'
  (when (and sys/macp (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  :config
  (with-no-warnings
    ;; persist views
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'ivy-views))

    ;; Highlight the selected item
    (defun my-ivy-format-function (cands)
      "Transform CANDS into a string for minibuffer."
      (if (display-graphic-p)
          (ivy-format-function-line cands)
        (ivy-format-function-arrow cands)))
    (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function)

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defconst my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))

    (defconst my-ivy-fly-back-commands
      '(self-insert-command
        ivy-forward-char ivy-delete-char delete-forward-char kill-word kill-sexp
        end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
        yank ivy-yank-word ivy-yank-char ivy-yank-symbol counsel-yank-pop))

    (defvar-local my-ivy-fly--travel nil)
    (defun my-ivy-fly-back-to-present ()
      (cond ((and (memq last-command my-ivy-fly-commands)
                  (equal (this-command-keys-vector) (kbd "M-p")))
             ;; repeat one time to get straight to the first history item
             (setq unread-command-events
                   (append unread-command-events
                           (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command my-ivy-fly-back-commands)
                 (equal (this-command-keys-vector) (kbd "M-n")))
             (unless my-ivy-fly--travel
               (delete-region (point) (point-max))
               (when (memq this-command '(ivy-forward-char
                                          ivy-delete-char delete-forward-char
                                          kill-word kill-sexp
                                          end-of-line mwim-end-of-line
                                          mwim-end-of-code-or-line
                                          mwim-end-of-line-or-code))
                 (insert (ivy-cleanup-string ivy-text))
                 (when (memq this-command '(ivy-delete-char
                                            delete-forward-char
                                            kill-word kill-sexp))
                   (beginning-of-line)))
               (setq my-ivy-fly--travel t)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (insert (propertize
                 (save-excursion
		           (set-buffer (window-buffer (minibuffer-selected-window)))
		           (ivy-thing-at-point))
                 'face 'shadow))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)
        (beginning-of-line)))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

    ;;
    ;; Improve search experience of `swiper' and `counsel'
    ;;
    (defun my-ivy-switch-to-swiper (&rest _)
      "Switch to `swiper' with the current input."
      (ivy-quit-and-run (swiper ivy-text)))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (ivy-quit-and-run (swiper-isearch ivy-text)))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (ivy-quit-and-run (swiper-all ivy-text)))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (interactive)
      (ivy-exit-with-action #'rg-dwim))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (ivy-quit-and-run (counsel-rg ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (ivy-quit-and-run (counsel-git-grep ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (ivy-quit-and-run (counsel-find-file ivy-text)))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (ivy-quit-and-run (counsel-fzf ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (ivy-quit-and-run (counsel-git ivy-text)))

    (defun my-ivy-switch-to-list-bookmarks (&rest _)
      "Switch to `list-bookmarks'."
      (ivy-quit-and-run (call-interactively #'list-bookmarks)))

    (defun my-ivy-switch-to-list-colors (&rest _)
      "Switch to `list-colors-display'."
      (ivy-quit-and-run (list-colors-display)))

    (defun my-ivy-switch-to-list-packages (&rest _)
      "Switch to `list-packages'."
      (ivy-quit-and-run (list-packages)))

    (defun my-ivy-switch-to-list-processes (&rest _)
      "Switch to `list-processes'."
      (ivy-quit-and-run (list-processes)))

    (defun my-ivy-copy-library-path (lib)
      "Copy the full path of LIB."
      (let ((path (find-library-name lib)))
        (kill-new path)
        (message "Copied path: \"%s\"." path)))

    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
      (interactive)
      (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
          (my-ivy-switch-to-counsel-rg)
        (my-ivy-switch-to-swiper-isearch)))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
      (bind-key "<M-return>" #'my-ivy-switch-to-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-ivy-switch-to-rg-dwim counsel-ag-map))

    (defun my-swiper-toggle-swiper-isearch ()
      "Toggle `swiper' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
            (swiper ivy-text)
          (swiper-isearch ivy-text))))
    (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

    (defun my-counsel-find-file-toggle-fzf ()
      "Toggle `counsel-fzf' with the current `counsel-find-file' input."
      (interactive)
      (ivy-quit-and-run
        (counsel-fzf (or ivy-text "") default-directory)))
    (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)

    (defun my-counsel-toggle ()
      "Toggle `counsel' commands and original commands."
      (interactive)
      (pcase (ivy-state-caller ivy-last)
        ('counsel-bookmark (my-ivy-switch-to-list-bookmarks))
        ('counsel-colors-emacs (my-ivy-switch-to-list-colors))
        ('counsel-colors-web (my-ivy-switch-to-list-colors))
        ('counsel-list-processes (my-ivy-switch-to-list-processes))
        ('counsel-package (my-ivy-switch-to-list-packages))
        (_ (ignore))))
    (bind-key "<C-return>" #'my-counsel-toggle ivy-minibuffer-map)

    ;; More actions
    (ivy-add-actions
     #'swiper-isearch
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'swiper
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'swiper-all
     '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
       ("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")))

    (ivy-add-actions
     #'counsel-rg
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

    (ivy-add-actions
     #'counsel-git-grep
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("r" my-ivy-switch-to-rg-dwim "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'counsel-find-file
     '(("g" my-ivy-switch-to-counsel-git "git")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     #'counsel-git
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-fzf
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("g" my-ivy-switch-to-counsel-git "git")))

    (ivy-add-actions
     'counsel-find-library
     '(("p" my-ivy-copy-library-path "copy path")))

    (ivy-add-actions
     'counsel-load-library
     '(("p" my-ivy-copy-library-path "copy path")))

    (ivy-add-actions
     #'counsel-bookmark
     '(("l" my-ivy-switch-to-list-bookmarks "list")))

    (ivy-add-actions
     #'counsel-colors-emacs
     '(("l" my-ivy-switch-to-list-colors "list")))

    (ivy-add-actions
     #'counsel-colors-web
     '(("l" my-ivy-switch-to-list-colors "list")))

    (ivy-add-actions
     #'counsel-package
     '(("l" my-ivy-switch-to-list-packages "list packages")))

    (ivy-add-actions
     #'counsel-list-processes
     '(("l" my-ivy-switch-to-list-processes "list"))))

  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))

  ;; Avy integration
  (use-package ivy-avy
    :bind (:map ivy-minibuffer-map
           ("C-'" . ivy-avy)))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :init
    (setq ivy-read-action-function 'ivy-hydra-read-action)

    (when (childframe-completion-workable-p)
      (setq hydra-hint-display-type 'posframe)

      (with-no-warnings
        (defun my-hydra-posframe-prettify-string (fn str)
          (funcall fn (concat (propertize "\n" 'face '(:height 0.3))
                              str
                              (propertize "\n\n" 'face '(:height 0.3)))))
        (advice-add #'hydra-posframe-show :around #'my-hydra-posframe-prettify-string)

        (defun ivy-hydra-poshandler-frame-center-below (info)
          (let (ivy-posframe-visible-p
                (pos (posframe-poshandler-frame-center-near-bottom info)))
            (catch 'break
              (dolist (frame (visible-frame-list))
                (when (string= (car (frame-parameter frame 'posframe-buffer))
                               ivy-posframe-buffer)
                  (setq ivy-posframe-visible-p t)
                  (throw 'break t))))
            (cons
             (car pos)
             (- (cdr pos)
                (if ivy-posframe-visible-p
                    (- (plist-get info :posframe-height)
                       (plist-get hydra-posframe-show-params :internal-border-width))
                  0)))))

        (defun my-ivy-posframe-read-action-by-key (actions)
          "Ivy-posframe's `ivy-read-action-by-key'. Use `ivy-hydra-read-action' instead."
          (ivy-hydra-read-action actions))
        (advice-add #'ivy-posframe-read-action-by-key
                    :override #'my-ivy-posframe-read-action-by-key)

        (defun hydra-set-posframe-show-params ()
          "Set hydra-posframe style."
          (setq hydra-posframe-show-params
                `(:left-fringe 10
                  :right-fringe 10
                  :internal-border-width 3
                  :internal-border-color ,(face-background 'posframe-border nil t)
                  :background-color ,(face-background 'tooltip nil t)
                  :lines-truncate t
                  :poshandler ivy-hydra-poshandler-frame-center-below)))
        (hydra-set-posframe-show-params)
        (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet))

  ;; Quick launch apps
  (cond
   (sys/linux-x-p
    (bind-key "s-<f6>" #'counsel-linux-app counsel-mode-map))
   (sys/macp
    (use-package counsel-osx-app
      :bind (:map counsel-mode-map
             ("s-<f6>" . counsel-osx-app)))))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
           ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
           ("C-c c T" . counsel-tramp)))

  ;; Support pinyin in Ivy
  ;; Input prefix ':' to match pinyin
  ;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  (use-package pinyinlib
    :autoload pinyinlib-build-regexp-string
    :init
    (with-no-warnings
      (defun my-pinyinlib-build-regexp-string (str)
        "Build a pinyin regexp sequence from STR."
        (cond ((equal str ".*") ".*")
              (t (pinyinlib-build-regexp-string str t))))

      (defun my-pinyin-regexp-helper (str)
        "Construct pinyin regexp for STR."
        (cond ((equal str " ") ".*")
              ((equal str "") nil)
              (t str)))

      (defun pinyin-to-utf8 (str)
        "Convert STR to UTF-8."
        (cond ((equal 0 (length str)) nil)
              ((equal (substring str 0 1) "!")
               (mapconcat
                #'my-pinyinlib-build-regexp-string
                (remove nil (mapcar
                             #'my-pinyin-regexp-helper
                             (split-string
                              (replace-regexp-in-string "!" "" str )
                              "")))
                ""))
              (t nil)))

      (defun my-ivy--regex-pinyin (fn str)
        "The regex builder advice to support pinyin."
        (or (pinyin-to-utf8 str)
            (funcall fn str)))
      (advice-add #'ivy--regex-plus :around #'my-ivy--regex-pinyin)
      (advice-add #'ivy--regex-ignore-order :around #'my-ivy--regex-pinyin))))

;; Use Ivy to open recent directories
(use-package ivy-dired-history
  :demand t
  :after dired
  :defines (savehist-additional-variables desktop-globals-to-save)
  :bind (:map dired-mode-map
         ("," . dired))
  :init
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'ivy-dired-history-variable)))


;; `projectile' integration
(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (when (executable-find "ugrep")
    (setq counsel-projectile-grep-base-command "ugrep --color=never -rnEI %s")))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init (setq all-the-icons-ivy-rich-icon centaur-icon)
  :config
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'centaur-load-theme
             '(:columns
               ((all-the-icons-ivy-rich-theme-icon)
                (ivy-rich-candidate))
               :delimiter "\t"))
  (all-the-icons-ivy-rich-reload))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook ((counsel-projectile-mode . ivy-rich-mode) ; MUST after `counsel-projectile'
         (ivy-rich-mode . ivy-rich-project-root-cache-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

;; Display completion in child frame
(when (childframe-completion-workable-p)
  (use-package ivy-posframe
    :diminish
    :custom-face
    (ivy-posframe-border ((t (:inherit posframe-border))))
    :hook (ivy-mode . ivy-posframe-mode)
    :init
    (setq ivy-height 15                 ; Use bigger minibuffer height for child frame
          ivy-posframe-border-width 3
          ivy-posframe-parameters '((left-fringe . 8)
                                    (right-fringe . 8)))
    :config
    (with-no-warnings
      ;; HACK: hide minibuffer with same colors
      (defun my-ivy-posframe--minibuffer-setup (fn &rest args)
        "Advice function of FN, `ivy--minibuffer-setup' with ARGS."
        (if (not (display-graphic-p))
            (apply fn args)
          (let ((ivy-fixed-height-minibuffer nil))
            (apply fn args))
          (when (and ivy-posframe-hide-minibuffer
                     (posframe-workable-p)
                     (string-match-p "^ivy-posframe" (symbol-name ivy--display-function)))
            (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
              (overlay-put ov 'window (selected-window))
              (overlay-put ov 'ivy-posframe t)
              (overlay-put ov 'face
                           (let* ((face (if (or (bound-and-true-p solaire-global-mode)
                                                (bound-and-true-p solaire-mode))
                                            'solaire-default-face
                                          'default))
                                  (bg-color (face-background face nil t)))
                             `(:background ,bg-color :foreground ,bg-color
                               :box nil :underline nil
                               :overline nil :strike-through nil)))
              (setq-local cursor-type nil)))))
      (advice-add #'ivy-posframe--minibuffer-setup :override #'my-ivy-posframe--minibuffer-setup)

      ;; Prettify the buffer
      (defun my-ivy-posframe--prettify-buffer (&rest _)
        "Add top and bottom margin to the prompt."
        (with-current-buffer ivy-posframe-buffer
          (goto-char (point-min))
          (insert (propertize "\n" 'face '(:height 0.3)))
          (goto-char (point-max))
          (insert (propertize "\n" 'face '(:height 0.3)))))
      (advice-add #'ivy-posframe--display :after #'my-ivy-posframe--prettify-buffer)

      ;; Adjust the postion
      (defun ivy-posframe-display-at-frame-center-near-bottom (str)
        (ivy-posframe--display str #'posframe-poshandler-frame-center-near-bottom))
      (setf (alist-get t ivy-posframe-display-functions-alist)
            #'ivy-posframe-display-at-frame-center-near-bottom))))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
