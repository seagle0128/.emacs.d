;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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
;; Define functions.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; Revert buffer
(declare-function flycheck-buffer 'flycheck)
(declare-function flymake-start 'flymake)
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(bind-key "s-r" #'revert-this-buffer)

;; Copy file name
(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (message "WARNING: Current buffer is not attached to a file!")))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load-file user-init-file))
(defalias 'centaur-reload-init-file 'reload-init-file)
(bind-key "C-c C-l" #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Centaur Emacs."
  (interactive)
  (browse-url centaur-homepage))

;; Open custom file
(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example
         (expand-file-name "custom-example.el" user-emacs-directory)))
    (unless (file-exists-p custom-file)
      (if (file-exists-p custom-example)
          (copy-file custom-example custom-file)
        (error "Unable to find \"%s\"" custom-example)))
    (find-file custom-file)))

(defun centaur-read-mode ()
  "Read articles with better views."
  (when (fboundp 'olivetti-mode)
    (olivetti-mode t))
  (text-scale-set +2))



;; Update
(defun update-config ()
  "Update Centaur Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating configurations...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating configurations...done"))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'centaur-update-config 'update-config)

(declare-function upgrade-packages 'init-package)
(declare-function async-inject-variables 'async)
(defun update-packages (&optional sync)
  "Refresh package contents and update all packages.

If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (message "Updating packages...")
  (if (and (not sync)
           (require 'async nil t))
      (async-start
       `(lambda ()
          ,(async-inject-variables "\\`\\(load-path\\)\\'")
          (require 'init-package)
          (upgrade-packages)
          (with-current-buffer auto-package-update-buffer-name
            (buffer-string)))
       (lambda (result)
         (message "%s" result)
         (message "Updating packages...done")))
    (progn
      (upgrade-packages)
      (message "Updating packages...done"))))
(defalias 'centaur-update-packages 'update-packages)

(defun update-config-and-packages(&optional sync)
  "Update confgiurations and packages.

If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (message "This will update Centaur Emacs to the latest")
  (if (and (not sync)
           (require 'async nil t))
      (async-start
       `(lambda ()
          ,(async-inject-variables "\\`\\(load-path\\)\\'")
          (require 'init-package)
          (require 'init-funcs)
          (update-config)
          (update-packages t)
          (with-current-buffer auto-package-update-buffer-name
            (buffer-string)))
       (lambda (result)
         (message "%s" result)
         (message "Done. Restart to complete process")))
    (progn
      (update-config)
      (update-packages t)
      (message "Done. Restart to complete process"))))
(defalias 'centaur-update 'update-config-and-packages)

(defun update-all()
  "Update dotfiles, org files, Emacs confgiurations and packages to the latest versions ."
  (interactive)
  (update-org)
  (update-dotfiles)
  (update-config-and-packages))
(defalias 'centaur-update-all 'update-all)

(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'centaur-update-dotfiles 'update-dotfiles)

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'centaur-update-org 'update-org)

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

;;
;; UI
;;

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun centaur--standardize-theme (theme)
  "Standardize THEME."
  (pcase theme
    ('default 'doom-one)
    ('classic 'doom-molokai)
    ('colorful 'doom-snazzy)
    ('dark 'doom-palenight)
    ('light 'doom-one-light)
    ('day 'doom-opera-light)
    ('night 'doom-city-lights)
    (_ (or theme 'doom-one))))

(defun centaur-compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name (centaur--standardize-theme theme))))

(defun centaur-load-theme (theme)
  "Set color THEME."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             '(default classic dark light daylight)))))
  (let ((theme (centaur--standardize-theme theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun centuar-dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun centuar-current-theme ()
  "The current enabled theme."
  (car custom-enabled-themes))

;;
;; Network Proxy
;;

(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" centaur-proxy)
    (message "No HTTP proxy")))



(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,centaur-proxy)
                             ("https" . ,centaur-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defvar socks-noproxy)
(defvar socks-server)
(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if socks-noproxy
      (message "Current SOCKS%d proxy is %s:%d"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1086 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
