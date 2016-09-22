;;; magithub-ci.el --- Show CI status as a magit-status header  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide the CI status of "origin" in the Magit status buffer.

;;; Code:

;; TODO

;; - go back in the log for the most recent commit without [ci skip]
;;   hub checks status of the current commit which will have 'no-status
;;   when skipped -- probably a good idea to report the commit that was
;;   actually tested
;; - docstrings!

(require 'magit)
(require 'magit-section)

(declare-function "magithub--command-output" 'magithub)
(declare-function "magithub-github-repository-p" 'magithub)

(defun magithub-maybe-insert-ci-status-header ()
  "If this is a GitHub repository, insert the CI status header."
  (when (magithub-github-repository-p)
    (magithub-insert-ci-status-header)))

(defun magithub-ci-status (&optional ignore-ci-skips)
  "One of 'success, 'error, 'failure, 'pending, or 'no-status."
  (with-temp-message "Updating CI status..."
    (let* ((last-commit (when ignore-ci-skips (magithub-ci-status--last-commit)))
           (output (car (magithub--command-output "ci-status" last-commit)))
           (output (replace-regexp-in-string "\s" "-" output))
           (status (intern output)))
      (if (and (not ignore-ci-skips) (eq status 'no-status))
          (magithub-ci-status t)
        status))))

(defun magithub-ci-status--last-commit ()
  "Find the commit considered to have the current CI status.
Right now, this finds the most recent commit without

    [ci skip]

or

    [skip ci]

in the commit message.

See the following resources:

 - https://docs.travis-ci.com/user/customizing-the-build#Skipping-a-build
 - https://circleci.com/docs/skip-a-build/"
  (let* ((args '("--invert-grep"
                 "--grep=\\[ci skip\\]"
                 "--grep=\\[skip ci\\]"
                 "--format=oneline"
                 "--max-count=1"))
         (output (magit-git-lines "log" args)))
    (car (split-string (car output)))))

(defvar magithub-ci-status-alist
  '((no-status . "None")
    (error . "Error")
    (failure . "Failure")
    (pending . "Pending")
    (success . "Success")))

(defface magithub-ci-no-status
  '((((class color)) :inherit magit-dimmed))
  "Face used when CI status is `no-status'."
  :group 'magithub-faces)

(defface magithub-ci-error
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `error'."
  :group 'magithub-faces)

(defface magithub-ci-pending
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `pending'."
  :group 'magithub-faces)

(defface magithub-ci-success
  '((((class color)) :inherit magit-signature-good))
  "Face used when CI status is `success'."
  :group 'magithub-faces)

(defface magithub-ci-failure
  '((((class color)) :inherit magit-signature-bad))
  "Face used when CI status is `'"
  :group 'magithub-faces)

(defface magithub-ci-unknown
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `unknown'."
  :group 'magithub-faces)

(defun magithub-ci-visit ()
  "Browse the CI.
Sets up magithub.ci.url if necessary."
  (interactive)
  (let ((var-value (magit-get "magithub" "ci" "url")))
    (unless var-value
      (magit-set
       (setq var-value (read-from-minibuffer "I don't know the CI URL yet -- what is it?  I'll remember: ")
             var-value (if (string-empty-p var-value) nil var-value))
       "magithub" "ci" "url"))
    (browse-url var-value)))

(defvar magit-magithub-ci-status-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-ci-visit)
    map)
  "Keymap for `magithub-ci-status' header section.")

(defun magithub-insert-ci-status-header ()
  (let* ((status (magithub-ci-status))
         (face   (intern (format "magithub-ci-%s"
                                 (symbol-name status)))))
    (magit-insert-section (magithub-ci-status)
      (insert (format "%-10s" "CI: "))
      (insert (propertize
               (cdr (assq status magithub-ci-status-alist))
               'face (if (facep face) face 'magithub-ci-unknown)))
      (insert ?\n))))

(defun magithub-toggle-ci-status-header ()
  (interactive)
  (if (memq #'magithub-maybe-insert-ci-status-header magit-status-headers-hook)
      (remove-hook 'magit-status-headers-hook #'magithub-maybe-insert-ci-status-header)
    (add-hook 'magit-status-headers-hook #'magithub-maybe-insert-ci-status-header t))
  (if (derived-mode-p major-mode 'magit-status-mode)
      (magit-refresh)))

(magithub-toggle-ci-status-header)

(provide 'magithub-ci)
;;; magithub-ci.el ends here
