(setq inhibit-startup-screen t)
(require 'init-meow)
(require 'posframe)
(global-set-key (kbd "<f5>") 'vterm-other-window)
(use-package rime
  :config
  (setq rime-disable-predicates
        '(meow-normal-mode-p
          meow-motion-mode-p
          meow-keypad-mode-p))
  (setq rime-posframe-properties
        (list :font "WenQuanYi Micro Hei Mono"
              :internal-border-width 10))
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/.local/share/fcitx5/rime"))

(use-package org-roam
  :custom
  (org-roam-directory "~/Org/Notes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org
  :config
  (setq org-directory "~/Org/")
  (setq org-capture-templates nil)
  (setq org-capture-templates '(
                                ("t" "Task" entry (file+headline "~/Org/task.org" "Task" )
                                 "* TODO [#A] %i%? \nSCHEDULED: %T " :empty-lines 1)
                                ("s" "School" entry (file+headline "~/Org/task.org" "School" )
                                 "* TODO [#A] %i%? \nDEADLINE: %T " :empty-lines 1)
                                ("w" "Work" entry (file+headline "~/Org/task.org" "Work" )
                                 "* TODO [#B] %i%? \nSCHEDULED: %t " :empty-lines 1)
                                ("p" "Project" entry (file "~/Org/project.org" )
                                 "* TODO [#B] %i%?" :empty-lines 1)
                                ("n" "Routine" entry (file "~/Org/routine.org")
                                 "* TODO [#C] %i%?" :empty-lines 1)
                                ("a" "Plan" entry (file "~/Org/plan.org")
                                 "* TODO [#C] %i%?" :empty-lines 1)
                                ))
  (setq org-agenda-files
        '("~/Org/task.org"
          "~/Org/project.org"
          "~/Org/plan.org"
          "~/Org/routine.org"
          "~/Org/notes.org")))
(use-package dired
  :config
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-AGhlv --group-directories-first --time-style=long-iso"))

(use-package dirvish
  :custom
  ;; Feel free to replace `all-the-icons' with `vscode-icon'.
  (dirvish-attributes '(expanded-state all-the-icons file-size))
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)
  ;; Go back home? Just press `bh'
  (dirvish-bookmarks-alist
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  ;; List directories that has over 10000 files asynchronously
  ;; This feature is disabled by default
  ;; (dirvish-async-listing-threshold 10000)
  :config
  ;; Place this line under :init to ensure the overriding at startup, see #22
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  ;; In case you want the details at startup like `dired'
  ;; :hook
  ;; (dirvish-mode . (lambda () (dired-hide-details-mode -1)))
  :bind
  (("C-x C-d" . dirvish)
   ("C-x d" . dirvish-roam)
                                        ; Bind `dirvish', `dirvish-dired' and `dirvish-side' as you see fit
   :map dired-mode-map
   ("SPC" . dirvish-show-history)
   ("r"   . dirvish-roam)
   ("b"   . dirvish-goto-bookmark)
   ("f"   . dirvish-file-info-menu)
   ("M-a" . dirvish-mark-actions-menu)
   ("M-s" . dirvish-setup-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ([remap dired-summary] . dirvish-dispatch)
   ([remap dired-do-copy] . dirvish-yank)
   ([remap mode-line-other-buffer] . dirvish-other-buffer)))

;;;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;;;(require 'eaf)
;;;(require 'eaf-browser)
;;;(require 'eaf-pdf-viewer)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
