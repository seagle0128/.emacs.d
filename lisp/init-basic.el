;; init-basic.el --- Better default configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

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
;; Better defaults.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Personal information
(setq user-full-name centaur-full-name
      user-mail-address centaur-mail-address)

(with-no-warnings
  ;; Key Modifiers
  (cond
   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper
    ;; (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super     ; Left Windows key
          w32-apps-modifier 'hyper)       ; Menu/App key
    (w32-register-hot-key [s-t]))
   (sys/mac-port-p
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo))))

  ;; Optimization
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO workload
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  ;; Garbage Collector Magic Hack
  (use-package gcmh
    :diminish
    :init
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold #x1000000) ; 16MB
    (gcmh-mode 1)))

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Environment
(when (or sys/mac-x-p sys/linux-x-p (daemonp))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-variables '("PATH" "MANPATH")
          exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Start server
(use-package server
  :ensure nil
  :if centaur-server
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  ;; Prettify the process list
  (with-no-warnings
    (define-derived-mode process-menu-mode tabulated-list-mode "Process Menu"
      "Major mode for listing the processes called by Emacs."
      (setq tabulated-list-format `[("" ,(if (icons-displayable-p) 2 0))
                                    ("Process" 25 t)
			                        ("PID"      7 t)
			                        ("Status"   7 t)
                                    ;; 25 is the length of the long standard buffer
                                    ;; name "*Async Shell Command*<10>" (bug#30016)
			                        ("Buffer"  25 t)
			                        ("TTY"     12 t)
			                        ("Thread"  12 t)
			                        ("Command"  0 t)])
      (make-local-variable 'process-menu-query-only)
      (setq tabulated-list-sort-key (cons "Process" nil))
      (add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t))

    (defun list-processes--refresh ()
      "Recompute the list of processes for the Process List buffer.
Also, delete any process that is exited or signaled."
      (setq tabulated-list-entries nil)
      (dolist (p (process-list))
        (cond ((memq (process-status p) '(exit signal closed))
	           (delete-process p))
	          ((or (not process-menu-query-only)
	               (process-query-on-exit-flag p))
	           (let* ((icon
                       (or
                        (and (icons-displayable-p)
                             (all-the-icons-octicon "zap"
                                                    :height 1.0 :v-adjust -0.05
                                                    :face 'all-the-icons-lblue))
                        ""))
                      (buf (process-buffer p))
		              (type (process-type p))
		              (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
		              (name (process-name p))
                      (status (process-status p))
		              (status `(,(symbol-name status)
                                face ,(if (memq status '(stop exit closed failed))
                                          'error
                                        'success)))
		              (buf-label (if (buffer-live-p buf)
				                     `(,(buffer-name buf)
				                       face link
				                       help-echo ,(format-message
					                               "Visit buffer `%s'"
					                               (buffer-name buf))
				                       follow-link t
				                       process-buffer ,buf
				                       action process-menu-visit-buffer)
			                       "--"))
		              (tty `(,(or (process-tty-name p) "--")
                             face font-lock-doc-face))
		              (thread
                       `(,(cond
                           ((or
                             (null (process-thread p))
                             (not (fboundp 'thread-name))) "--")
                           ((eq (process-thread p) main-thread) "Main")
		                   ((thread-name (process-thread p)))
		                   (t "--"))
                         face font-lock-doc-face))
		              (cmd
		               `(,(if (memq type '(network serial pipe))
		                      (let ((contact (process-contact p t t)))
			                    (if (eq type 'network)
			                        (format "(%s %s)"
				                            (if (plist-get contact :type)
					                            "datagram"
				                              "network")
				                            (if (plist-get contact :server)
					                            (format
                                                 "server on %s"
					                             (if (plist-get contact :host)
                                                     (format "%s:%s"
						                                     (plist-get contact :host)
                                                             (plist-get
                                                              contact :service))
					                               (plist-get contact :local)))
				                              (format "connection to %s:%s"
					                                  (plist-get contact :host)
					                                  (plist-get contact :service))))
			                      (format "(serial port %s%s)"
				                          (or (plist-get contact :port) "?")
				                          (let ((speed (plist-get contact :speed)))
				                            (if speed
					                            (format " at %s b/s" speed)
				                              "")))))
		                    (mapconcat 'identity (process-command p) " "))
                         face completions-annotations)))
	             (push (list p (vector icon name pid status buf-label tty thread cmd))
		               tabulated-list-entries)))))
      (tabulated-list-init-header))))

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(when emacs/>=27p
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Fullscreen
(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
