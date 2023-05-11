;;; init-persp.el --- Initialize perspectives configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2023 Vincent Zhang

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
;; perspectives configurations.
;;

;;; Code:

(require 'init-custom)
(require 'init-funcs)

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers)
  :autoload (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (persp-mode . persp-load-frame)
         (kill-emacs . persp-save-frame))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time (if centaur-dashboard 0 1.0))
  :config
  ;; Save and load frame parameters (size & position)
  (defvar persp-frame-file (expand-file-name "persp-frame" persp-save-dir)
    "File of saving frame parameters.")

  (defun persp-save-frame ()
    "Save the current frame parameters to file."
    (interactive)
    (when (and (display-graphic-p) centaur-restore-frame-geometry persp-mode)
      (condition-case error
          (with-temp-buffer
            (erase-buffer)
            (insert
             ";;; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
             ";;; This is the previous frame parameters.\n"
             ";;; Last generated " (current-time-string) ".\n"
             "(setq initial-frame-alist\n"
             (format "      '((top . %d)\n" (eval (frame-parameter nil 'top)))
             (format "        (left . %d)\n" (eval (frame-parameter nil 'left)))
             (format "        (width . %d)\n" (eval (frame-parameter nil 'width)))
             (format "        (height . %d)\n" (eval (frame-parameter nil 'height)))
             (format "        (fullscreen . %s)))\n" (frame-parameter nil 'fullscreen)))
            (write-file persp-frame-file))
        (error
         (warn "persp frame: %s" (error-message-string error))))))

  (defun persp-load-frame ()
    "Load frame with the previous frame's geometry."
    (interactive)
    (when (and (display-graphic-p) centaur-restore-frame-geometry persp-mode)
      (condition-case error
          (progn
            (fix-fullscreen-cocoa)
            (load persp-frame-file nil t)

            ;; NOTE: Only usable in `emacs-startup-hook' while not `window-setup-hook'.
            (add-hook 'emacs-startup-hook
                      (lambda ()
                        "Adjust initial frame position."
                        ;; Handle multiple monitors gracefully
                        (when (or (>= (eval (frame-parameter nil 'top)) (display-pixel-height))
                                  (>= (eval (frame-parameter nil 'left)) (display-pixel-width)))
                          (set-frame-parameter nil 'top 0)
                          (set-frame-parameter nil 'left 0)))))
        (error
         (warn "persp frame: %s" (error-message-string error))))))

  (with-no-warnings
    ;; Don't save if the state is not loaded
    (defvar persp-state-loaded nil
      "Whether the state is loaded.")

    (defun my-persp-after-load-state (&rest _)
      (setq persp-state-loaded t))
    (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (add-hook 'find-file-hook #'my-persp-after-load-state)))

    (defun my-persp-asave-on-exit (fn &optional interactive-query opt)
      (if persp-state-loaded
          (funcall fn interactive-query opt)
        t))
    (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit))

  ;; Don't save dead or temporary buffers
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore dead and unneeded buffers."
              (or (not (buffer-live-p b))
                  (string-prefix-p " *" (buffer-name b)))))
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore temporary buffers."
              (let ((bname (file-name-nondirectory (buffer-name b))))
                (or (string-prefix-p ".newsrc" bname)
                    (string-prefix-p "magit" bname)
                    (string-prefix-p "COMMIT_EDITMSG" bname)
                    (string-prefix-p "Pfuture-Callback" bname)
                    (string-prefix-p "treemacs-persist" bname)
                    (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                    (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Ivy Integration
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 (lambda (b)
                   (when persp-mode
                     (if-let ((persp (get-current-persp)))
                         (not (persp-contain-buffer-p b persp))
                       nil)))))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory)))

;; Project integration
(when emacs/>=27p
  (use-package persp-mode-project-bridge
    :autoload (persp-mode-project-bridge-find-perspectives-for-all-buffers
               persp-mode-project-bridge-kill-perspectives)
    :hook
    (persp-mode-project-bridge-mode . (lambda ()
                                        (if persp-mode-project-bridge-mode
                                            (persp-mode-project-bridge-find-perspectives-for-all-buffers)
                                          (persp-mode-project-bridge-kill-perspectives))))
    (persp-mode . persp-mode-project-bridge-mode)
    :init (when (icons-displayable-p)
            (setq persp-mode-project-bridge-persp-name-prefix ""))
    :config
    (with-no-warnings
      ;;Add buffer while `find-file'
      (defun persp-mode-project-bridge-hook-find-file (&rest _args)
        (let ((persp
               (persp-mode-project-bridge-find-perspective-for-buffer
                (current-buffer))))
          (when persp
            (persp-add-buffer (current-buffer) persp nil nil))))
      (add-hook 'find-file-hook
                #'persp-mode-project-bridge-hook-find-file)

      ;; Allow saving to files
      (defun my-persp-mode-project-bridge-add-new-persp (name)
        (let ((persp (persp-get-by-name name *persp-hash* :nil)))
          (if (eq :nil persp)
              (prog1
                  (setq persp (persp-add-new name))
                (when persp
                  (set-persp-parameter 'persp-mode-project-bridge t persp)
                  (persp-add-buffer (cl-remove-if-not #'get-file-buffer (project-files (project-current)))
                                    persp nil nil)))
            persp)))
      (advice-add #'persp-mode-project-bridge-add-new-persp
                  :override #'my-persp-mode-project-bridge-add-new-persp))))

(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
