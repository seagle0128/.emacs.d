;; init-check.el --- Initialize check configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2009-2025 Vincent Zhang

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
;; Check configurations.
;;

;;; Code:

(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook prog-mode
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-margin-indicator-position 'right-margin)
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))

;; Display Flymake errors with overlays
(use-package flyover
  :diminish
  :hook flymake-mode
  :custom (flyover-checkers '(flymake))
  :config
  ;; FIXME: see https://github.com/konrad1977/flyover/issues/30#issuecomment-3624168811
  (defun flyover--maybe-display-errors-debounced (&rest _)
    "Debounced messages of `flyover--maybe-display-errors`."
    (condition-case err
        (progn
          (when flyover--debounce-timer
            (cancel-timer flyover--debounce-timer))
          (setq flyover--debounce-timer
                (run-with-idle-timer flyover-debounce-interval nil
                                     #'flyover--maybe-display-errors)))
      (error
       (message "Error in debounced display: %S" err)
       (setq flyover--debounce-timer nil)))))

(provide 'init-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
