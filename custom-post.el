(use-package meow)
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("p" . meow-prev)
   '("n" . meow-next)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . "C-c C-x")
   '("d" . meow-kill)
   '("D" . meow-delete)
   '("e" . meow-left)
   '("E" . meow-left-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-search)
   '("H" . meow-next-word)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-insert)
   '("K" . meow-open-above)
   '("l" . undo-tree-undo)
   '("L" . undo-tree-redo)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . "C-x")
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-line)
   '("V" . meow-goto-line)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . "C-y")
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
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
