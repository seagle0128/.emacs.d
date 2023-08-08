;;; cape-yasnippet.el --- Yasnippet Completion at Point Extension -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: August 11, 2022
;; Modified: August 11, 2022
;; Version: 0.0.3
;; Homepage: https://github.com/elken/cape-yasnippet
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Yasnippet Completion at Point Extension to lookup snippets by name
;;
;; Simply add to the list of existing `completion-at-point-functions' thus:
;;    (add-to-list 'completion-at-point-functions #'cape-yasnippet)
;;
;; If you prefer to have the lookup done by name rather than key, set
;; `cape-yasnippet-lookup-by'.
;;
;;; Code:

(require 'cape)
(require 'yasnippet)
(require 'cl-lib)
(require 'subr-x)

(defgroup cape-yasnippet nil
  "Yasnippet CAPE."
  :group 'cape)

(defcustom cape-yasnippet-lookup-by 'key
  "The method in which to lookup candidates by."
  :type '(choice
          (const :tag "Key" key)
          (const :tag "Name" name)))

(defvar cape-yasnippet--properties
  (list :annotation-function (lambda (snippet) (get-text-property 0 'yas-annotation snippet))
        :company-kind (lambda (_) 'snippet)
        :exit-function (lambda (cand status)
                         (when (string= "finished" status)
                           (when-let ((snippet (cape-yasnippet--lookup-snippet cand)))
                             (delete-char (* -1 (length cand)))
                             (yas-expand-snippet snippet))))
        :exclusive 'no)
  "Completion extra properties for `cape-yasnippet'.")

(defun cape-yasnippet--lookup-snippet (name)
  "Get the snippet called NAME in MODE's tables."
  (let ((yas-choose-tables-first nil)
        (yas-choose-keys-first nil))
    (cl-find name (yas--all-templates
                   (yas--get-snippet-tables major-mode))
             :key (pcase cape-yasnippet-lookup-by
                    ('key #'yas--template-key)
                    ('name #'yas--template-name))
             :test #'string=)))

(defun cape-yasnippet--key-prefixes ()
  "Mostly copied from `yas--templates-for-key-at-point'."
  (defvar yas-key-syntaxes)
  (save-excursion
    (let ((original (point))
          (methods yas-key-syntaxes)
          prefixes
          method)
      (while methods
        (unless (eq method (car methods))
          (goto-char original))
        (setq method (car methods))
        (cond ((stringp method)
               (skip-syntax-backward method)
               (setq methods (cdr methods)))
              ((functionp method)
               (unless (eq (funcall method original)
                           'again)
                 (setq methods (cdr methods))))
              (t
               (setq methods (cdr methods))
               (yas--warning "Invalid element `%s' in `yas-key-syntaxes'" method)))
        (let ((prefix (buffer-substring-no-properties (point) original)))
          (unless (equal prefix (car prefixes))
            (push prefix prefixes))))
      prefixes)))

(defun cape-yasnippet--completions-for-prefix (prefix key-prefix tables)
  "Get a completion candidate for PREFIX with KEY-PREFIX in TABLES."
  (cl-mapcan
   (lambda (table)
     (let ((keyhash (yas--table-hash table))
           (requirement (yas--require-template-specific-condition-p))
           res)
       (when keyhash
         (maphash
          (lambda (key value)
            (when (and (stringp key)
                       (string-prefix-p key-prefix key))
              (maphash
               (lambda (name template)
                 (when (yas--template-can-expand-p
                        (yas--template-condition template) requirement)
                   (push
                    (propertize (pcase cape-yasnippet-lookup-by
                                  ('key key)
                                  ('name name))
                                'yas-annotation name
                                'yas-template template
                                'yas-prefix-offset (- (length (pcase cape-yasnippet-lookup-by
                                                                ('key key)
                                                                ('name name)))
                                                      (length prefix)))
                    res)))
               value)))
          keyhash))
       res))
   tables))

(defun cape-yasnippet-candidates (&optional prefix)
  "Return a list of candidate snippets filtered by PREFIX."
  (pcase cape-yasnippet-lookup-by
    ('key
     (cl-loop with tables = (yas--get-snippet-tables)
              for key-prefix in (cape-yasnippet--key-prefixes)
              when (>= (length key-prefix) (length prefix))
              thereis (cape-yasnippet--completions-for-prefix prefix
                                                              key-prefix
                                                              tables)))
    ('name
     (thread-last (yas--get-snippet-tables)
                  (yas--all-templates)
                  (mapcar #'yas--template-name)
                  (cl-remove-if-not (lambda (cand)
                                      (and prefix
                                           (string-prefix-p prefix cand))))))
    (_ (error "Invalid value for cape-yasnippet-lookup-by: %s" cape-yasnippet-lookup-by))))

(defun cape-yasnippet--list (input)
  "Use INPUT to compute and filter a new cached table."
  (cons (apply-partially #'string-prefix-p input)
        (cape-yasnippet-candidates input)))

;;;###autoload
(defun cape-yasnippet (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-yasnippet)
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        `(,beg ,end
          ,(cape--table-with-properties
            (cape--cached-table beg end #'cape-yasnippet--list)
            :category 'cape-yasnippet)
          ,@cape-yasnippet--properties)))))

(provide 'cape-yasnippet)
;;; cape-yasnippet.el ends here
