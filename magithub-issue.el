;;; magithub-issue.el --- Browse issues with Magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Sean Allred

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

;; Jump to issues from `magit-status'!

;;; Code:

(require 'dash)
(require 'ghub+)
(require 'cl-lib)

(require 'magithub-core)

;; Core
(defun magithub--issue-list (&rest params)
  "Return a list of issues for the current repository.
The response is unpaginated, so avoid doing this with PARAMS that
will return a ton of issues.

See also `ghubp-get-repos-owner-repo-issues'."
  (cl-assert (cl-evenp (length params)))
  (magithub-cache :issues
    `(ghubp-unpaginate
      (ghubp-get-repos-owner-repo-issues
       ',(magithub-repo)
       ,@params))
    "Retrieving issue list..."))

(defun magithub-issue--issue-is-pull-p (issue)
  (not (null (alist-get 'pull_request issue))))

(defun magithub-issue--issue-is-issue-p (issue)
  (and (alist-get 'number issue)
       (not (magithub-issue--issue-is-pull-p issue))))

;; Finding issues and pull requests
(defun magithub-issues ()
  "Return a list of issue objects that are actually issues."
  (-filter #'magithub-issue--issue-is-issue-p
           (magithub--issue-list)))

(defun magithub-pull-requests ()
  "Return a list of issue objects that are actually pull requests."
  (-filter #'magithub-issue--issue-is-pull-p
           (magithub--issue-list)))

;; Sorting
(defcustom magithub-issue-sort-function
  #'magithub-issue-sort-ascending
  "Function used for sorting issues and pull requests in the
status buffer.  Should take two issue-objects as arguments."
  :type 'function
  :group 'magithub)

(defun magithub-issue-sort-ascending (a b)
  "Lower issue numbers come first."
  (< (plist-get a :number)
     (plist-get b :number)))

(defun magithub-issue-sort-descending (a b)
  "Higher issue numbers come first."
  (< (plist-get b :number)
     (plist-get a :number)))

(defun magithub-issue--sort (issues)
  "Sort ISSUES by `magithub-issue-sort-function'."
  (sort issues magithub-issue-sort-function))

;; Getting issues from the user
(defun magithub-issue--format-for-read (issue)
  "Format ISSUE as a string suitable for completion."
  (let-alist issue (format "%3d %s" .number .title)))

(defun magithub-issue--completing-read (prompt default preds)
  "Complete over all open pull requests returning its issue object.
If point is on a pull-request object, that object is selected by
default."
  (magithub--completing-read prompt (magithub--issue-list)
                             #'magithub-issue--format-for-read
                             (apply-partially #'magithub--satisfies-p preds)
                             t default))
(defun magithub-issue-completing-read-issues (&optional default)
  "Read an issue in the minibuffer with completion."
  (interactive (list (magithub-thing-at-point 'issue)))
  (magithub-issue--completing-read
   "Issue: " default (list #'magithub-issue--issue-is-issue-p)))
(defun magithub-issue-completing-read-pull-requests (&optional default)
  "Read a pull request in the minibuffer with completion."
  (interactive (list (magithub-thing-at-point 'pull-request)))
  (magithub-issue--completing-read
   "Pull Request: " default (list #'magithub-issue--issue-is-pull-p)))

(defun magithub-issue-find (number)
  "Return the issue or pull request with the given NUMBER."
  (-find (lambda (i) (= (alist-get 'number i) number))
         (magithub--issue-list :filter "all" :state "all")))

(defun magithub-issue (repo number)
  "Retrieve in REPO issue NUMBER."
  (magithub-cache :issues
    `(ghubp-get-repos-owner-repo-issues-number
      ',repo '((number . ,number)))
    (format "Getting issue %s#%d..." (magithub-repo-name repo) number)))

(defun magithub-issue-personal-note-file (issue-or-pr)
  "Return an absolute filename appropriate for ISSUE-OR-PR."
  (expand-file-name
   (format "magithub/notes/%d.org"
           (alist-get 'number issue-or-pr))
   (magit-git-dir)))

(magithub-interactive-issue-or-pr personal-note (issue-or-pr)
  "Write a personal note about %s.
This is stored in `magit-git-dir' and is unrelated to
`git-notes'."
  (let* ((num (alist-get 'number issue-or-pr))
         (note-file (magithub-issue-personal-note-file issue-or-pr)))
    (make-directory (file-name-directory note-file) t)
    (with-current-buffer (find-file note-file)
      (rename-buffer (format "*magithub note for #%d*" num)))))

(defun magithub-issue-has-personal-note-p (issue-or-pr)
  "Non-nil if a personal note exists for ISSUE-OR-PR."
  (let ((filename (magithub-issue-personal-note-file issue-or-pr)))
    (and (file-exists-p filename)
         (not (string-equal
               ""
               (string-trim
                (with-temp-buffer
                  (insert-file-contents-literally filename)
                  (buffer-string))))))))

(provide 'magithub-issue)
;;; magithub-issue.el ends here
