(setq inhibit-startup-screen t)
(require 'posframe)
(require 'init-meow)
(require 'init-rime)

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

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
