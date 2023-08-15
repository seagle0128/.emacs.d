;;; yasnippet-capf.el --- Yasnippet Completion At Point Function -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ellis Kenyő
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: August 11, 2022
;; Modified: August 11, 2022
;; Version: 0.0.3
;; Homepage: https://github.com/elken/yasnippet-capf
;; Package-Requires: ((emacs "25.1") (yasnippet "0.14.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Yasnippet Completion at Point Function to lookup snippets by name
;;
;; Simply add to the list of existing `completion-at-point-functions' thus:
;;    (add-to-list 'completion-at-point-functions #'yasnippet-capf)
;;
;; If you prefer to have the lookup done by name rather than key, set
;; `yasnippet-capf-lookup-by'.
;;
;;; Code:

(require 'thingatpt)
(require 'yasnippet)
(require 'cl-lib)
(require 'subr-x)

(defgroup yasnippet-capf nil
  "Yasnippet CAPF."
  :group 'completion)

(defcustom yasnippet-capf-lookup-by 'key
  "The method in which to lookup candidates by."
  :type '(choice
          (const :tag "Key" key)
          (const :tag "Name" name)))

(defvar yasnippet-capf--properties
  (list :annotation-function (lambda (snippet) (get-text-property 0 'yas-annotation snippet))
        :company-kind (lambda (_) 'snippet)
        :company-doc-buffer #'yasnippet-capf--doc-buffer
        :exit-function (lambda (_ status)
                         (when (string= "finished" status)
                           (yas-expand)))
        :exclusive 'no)
  "Completion extra properties for `yasnippet-capf'.")

(defun yasnippet-capf--doc-buffer (cand)
  "Calculate the expansion of the snippet for CAND.
Returns a buffer to be displayed by popupinfo."
  (when-let ((mode major-mode)
             (template (get-text-property 0 'yas-template cand)))
    (with-current-buffer (get-buffer-create "*yasnippet-capf-doc*")
      (erase-buffer)
      (yas-minor-mode)
      (insert "Expands to:" ?\n ?\n)
      (condition-case error
          (yas-expand-snippet (yas--template-content template))
        (error
         (message "Error expanding: %s" (error-message-string error))))
      (delay-mode-hooks
        (let ((inhibit-message t))
          (when (eq mode 'web-mode)
            (setq mode 'html-mode))
          (funcall mode)))
      (ignore-errors (font-lock-ensure))
      (current-buffer))))

(defun yasnippet-capf--lookup-snippet (name)
  "Get the snippet called NAME in MODE's tables."
  (let ((yas-choose-tables-first nil)
        (yas-choose-keys-first nil))
    (cl-find name (yas--all-templates
                   (yas--get-snippet-tables major-mode))
             :key (intern-soft (format "yas--template-%s" yasnippet-capf-lookup-by))
             :test #'string=)))

(defun yasnippet-capf--completions-for-prefix (prefix tables)
  "Get a completion candidate for PREFIX with KEY-PREFIX in TABLES."
  (let ((templates (yas--all-templates tables))
        (requirement (yas--require-template-specific-condition-p)))
    (mapcar (lambda (template)
              (let ((can-expand (yas--template-can-expand-p
                                 (yas--template-condition template) requirement))
                    (name (yas--template-name template))
                    (name-or-key
                     (funcall (intern-soft (format "yas--template-%s" yasnippet-capf-lookup-by)) template)))
                (when can-expand
                  (propertize name-or-key
                              'yas-annotation name
                              'yas-template template
                              'yas-prefix-offset (- (length name-or-key)
                                                    (length prefix))))))
            templates)))

(defun yasnippet-capf-candidates (&optional prefix)
  "Return a list of candidate snippets filtered by PREFIX."
  (pcase yasnippet-capf-lookup-by
    ('key
     (thread-last (yas--get-snippet-tables)
                  (yasnippet-capf--completions-for-prefix prefix)
                  (cl-remove-if #'null)))
    ('name
     (thread-last (yas--get-snippet-tables)
                  (yas--all-templates)
                  (mapcar #'yas--template-name)))
    (_ (error "Invalid value for yasnippet-capf-lookup-by: %s" yasnippet-capf-lookup-by))))

;;;###autoload
(defun yasnippet-capf (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions #'yasnippet-capf))
        (or (completion-at-point) (user-error "yasnippet-capf: No completions")))
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      `(,(match-beginning 0) ,(match-end 0)
        ,(completion-table-with-cache
          (lambda (input)
            (yasnippet-capf-candidates input)))
        ,@yasnippet-capf--properties))))

(provide 'yasnippet-capf)
;;; yasnippet-capf.el ends here
