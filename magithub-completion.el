;;; magithub-completion.el --- Completion using info provided by magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `completion-at-point' functions which complete issue
;; numbers etc when they are entered in commit messages.

;; Extended information is attached to completions so that `company'
;; can access it via the standard `company-capf' backend.

;;; Code:


(require 'magithub-settings)
(require 'magithub-issue)


;;;###autoload
(defun magithub-completion-complete-issues ()
  "A `completion-at-point' function which completes \"#123\" issue references.
Add this to `completion-at-point-functions' in buffers where you
want this to be available."
  (when (magithub-enabled-p)
    (when (looking-back "#\\([0-9]*\\)" (- (point) 10))
      (let ((start (match-beginning 1))
            (end (match-end 0))
            (prefix (match-string 1))
            completions)
        (dolist (i (magithub--issue-list))
          (let-alist i
            (let ((n (number-to-string .number)))
              (when (string-prefix-p prefix n)
                (push (propertize n :issue i) completions)))))
        (list start end (sort completions #'string<)
              :exclusive 'no
              :company-docsig (lambda (c)
                                (let-alist (get-text-property 0 :issue c)
                                  .title))
              :annotation-function (lambda (c)
                                     (let-alist (get-text-property 0 :issue c)
                                       .title))
              :company-doc-buffer (lambda (c)
                                    (save-window-excursion
                                      (magithub-issue-visit
				       (get-text-property 0 :issue c)))))))))

;;;###autoload
(defun magithub-completion-complete-users ()
  "A `completion-at-point' function which completes \"@user\" user references.
Add this to `completion-at-point-functions' in buffers where you
want this to be available.  The user list is currently simply the
list of all users who created issues or pull requests."
  (when (magithub-enabled-p)
    (when (looking-back "@\\([_-A-Za-z0-9]*\\)" (- (point) 30))
      (let ((start (match-beginning 1))
            (end (match-end 0))
            (prefix (match-string 1))
            completions)
        (dolist (i (magithub--issue-list))
          (let-alist i
            (when (string-prefix-p prefix .user.login)
              (let ((candidate (copy-sequence .user.login))
                    (association (and .author_association
                                      (not (string= "NONE" .author_association))
                                      .author_association)))
                (push (propertize candidate :user .user :association association)
                      completions)))))
        (list start end (sort (cl-remove-duplicates completions :test #'string=)
			      #'string<)
              :exclusive 'no
              :company-docsig (lambda (c) (get-text-property 0 :association c))
              :annotation-function (lambda (c) (get-text-property 0 :association c)))))))

;;;###autoload
(defun magithub-completion-enable ()
  "Enable completion of info from magithub in the current buffer."
  (make-local-variable 'completion-at-point-functions)
  (dolist (f '(magithub-completion-complete-issues
	       magithub-completion-complete-users))
    (add-to-list 'completion-at-point-functions f)))


(provide 'magithub-completion)
;;; magithub-completion.el ends here
