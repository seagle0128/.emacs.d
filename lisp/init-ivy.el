;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Vincent Zhang

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
;; Ivy configurations.
;;

;;; Code:

(require 'init-custom)
(require 'init-funcs)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s"   . swiper-isearch)
         ("C-r"   . swiper-isearch-backward)
         ("s-f"   . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap dired] . counsel-dired)
         ([remap set-variable] . counsel-set-variable)
         ([remap insert-char] . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf)

         ("C-x j"   . counsel-mark-ring)
         ("C-h F"   . counsel-faces)

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
         ("C-c o" . counsel-outline)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c B" . counsel-bookmarked-directory)
         ("C-c c F" . counsel-faces)
         ("C-c c L" . counsel-load-library)
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
         ("C-c c l" . counsel-locate)
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
         ("C-`" . ivy-avy)

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

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)

  ;; Better performance on Windows
  (when sys/win32p
    (setq ivy-dynamic-exhibit-delay-ms 200))

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and sys/macp (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))
  :config
  (with-no-warnings
    ;; Display an arrow with the selected item
    (defun my-ivy-format-function-arrow (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                          (>= (length str) 1)
                          (string= " " (substring str 0 1)))
                     ">"
                   "> ")
                 (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                          (>= (length str) 1)
                          (string= " " (substring str 0 1)))
                     " "
                   "  ")
                 str))
       cands
       "\n"))
    (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))
    (defvar-local my-ivy-fly--travel nil)

    (defun my-ivy-fly-back-to-present ()
      (cond ((and (memq last-command my-ivy-fly-commands)
                  (equal (this-command-keys-vector) (kbd "M-p")))
             ;; repeat one time to get straight to the first history item
             (setq unread-command-events
                   (append unread-command-events
                           (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command '(self-insert-command
                                      ivy-forward-char
                                      ivy-delete-char delete-forward-char
                                      end-of-line mwim-end-of-line
                                      mwim-end-of-code-or-line mwim-end-of-line-or-code
                                      yank ivy-yank-word counsel-yank-pop))
                 (equal (this-command-keys-vector) (kbd "M-n")))
             (unless my-ivy-fly--travel
               (delete-region (point) (point-max))
               (when (memq this-command '(ivy-forward-char
                                          ivy-delete-char delete-forward-char
                                          end-of-line mwim-end-of-line
                                          mwim-end-of-code-or-line
                                          mwim-end-of-line-or-code))
                 (insert (ivy-cleanup-string ivy-text))
                 (when (memq this-command '(ivy-delete-char delete-forward-char))
                   (beginning-of-line)))
               (setq my-ivy-fly--travel t)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (let* ((kbd (kbd "M-n"))
               (cmd (key-binding kbd))
               (future (and cmd
                            (with-temp-buffer
                              (when (ignore-errors
                                      (call-interactively cmd) t)
                                (buffer-string))))))
          (when future
            (save-excursion
              (insert (propertize (replace-regexp-in-string
                                   "\\\\_<" ""
                                   (replace-regexp-in-string
                                    "\\\\_>" ""
                                    future))
                                  'face 'shadow)))
            (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

    ;;
    ;; Improve search experience of `swiper' and `counsel'
    ;;
    (defun my-ivy-switch-to-swiper (&rest _)
      "Switch to `swiper' with the current input."
      (swiper ivy-text))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (swiper-isearch ivy-text))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (swiper-all ivy-text))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (rg-dwim default-directory))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (counsel-rg ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (counsel-git-grep ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (counsel-find-file ivy-text))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (counsel-fzf ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (counsel-git ivy-text))

    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
            (my-ivy-switch-to-counsel-rg)
          (my-ivy-switch-to-swiper-isearch))))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
      (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with the current input."
        (interactive)
        (ivy-quit-and-run
          (rg-dwim default-directory)))
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map))

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

    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with the current input."
      (interactive)
      (ivy-quit-and-run (my-ivy-switch-to-rg-dwim)))
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map)

    (defun my-swiper-toggle-swiper-isearch ()
      "Toggle `swiper' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
            (my-ivy-switch-to-swiper)
          (my-ivy-switch-to-swiper-isearch))))
    (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

    ;; More actions
    (ivy-add-actions
     'swiper-isearch
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'swiper
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'swiper-all
     '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
       ("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")))

    (ivy-add-actions
     'counsel-rg
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

    (ivy-add-actions
     'counsel-git-grep
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("r" my-ivy-switch-to-rg-dwim "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'counsel-find-file
     '(("g" my-ivy-switch-to-counsel-git "git")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-git
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-fzf
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("g" my-ivy-switch-to-counsel-git "git")))

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    ;; Integration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read)))

  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))

  ;; Better sorting and filtering
  (use-package prescient
    :commands prescient-persist-mode
    :init (prescient-persist-mode 1))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (setq ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist
          '((counsel-ag . ivy-prescient-non-fuzzy)
            (counsel-rg . ivy-prescient-non-fuzzy)
            (counsel-pt . ivy-prescient-non-fuzzy)
            (counsel-grep . ivy-prescient-non-fuzzy)
            (counsel-imenu . ivy-prescient-non-fuzzy)
            (counsel-yank-pop . ivy-prescient-non-fuzzy)
            (swiper . ivy-prescient-non-fuzzy)
            (swiper-isearch . ivy-prescient-non-fuzzy)
            (swiper-all . ivy-prescient-non-fuzzy)
            (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
            (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
            (insert-char . ivy-prescient-non-fuzzy)
            (counsel-unicode-char . ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder))
          ivy-prescient-sort-commands
          '(:not swiper swiper-isearch ivy-switch-buffer
            lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
            counsel-grep counsel-git-grep counsel-rg counsel-ag
            counsel-ack counsel-fzf counsel-pt counsel-imenu
            counsel-org-capture counsel-load-theme counsel-yank-pop
            counsel-recentf counsel-buffer-or-recentf))

    (ivy-prescient-mode 1))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :commands ivy-hydra-read-action
    :init (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

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
    :commands pinyinlib-build-regexp-string
    :init
    (with-no-warnings
      (defun ivy--regex-pinyin (str)
        "The regex builder wrapper to support pinyin."
        (or (pinyin-to-utf8 str)
            (and (fboundp 'ivy-prescient-non-fuzzy)
                 (ivy-prescient-non-fuzzy str))
            (ivy--regex-plus str)))

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

      (mapcar
       (lambda (item)
         (let ((key (car item))
               (value (cdr item)))
           (when (member value '(ivy-prescient-non-fuzzy
                                 ivy--regex-plus))
             (setf (alist-get key ivy-re-builders-alist)
                   #'ivy--regex-pinyin))))
       ivy-re-builders-alist))))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if (icons-displayable-p)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook (;; Must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
