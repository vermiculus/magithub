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
(require 'magithub-user)

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
  (let-alist issue-or-pr
    (expand-file-name
     (format "magithub/notes/%d.org" .number)
     (magit-git-dir))))

(magithub-interactive-issue-or-pr personal-note (issue-or-pr)
  "Write a personal note about %s.
This is stored in `magit-git-dir' and is unrelated to
`git-notes'."
  (if (null issue-or-pr)
      (error "No issue or pull request here")
    (let-alist issue-or-pr
      (let ((note-file (magithub-issue-personal-note-file issue-or-pr)))
        (make-directory (file-name-directory note-file) t)
        (with-current-buffer (find-file note-file)
          (rename-buffer (format "*magithub note for #%d*" .number)))))))

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

(defun magithub-issue-repo (issue)
  "Get a repository object from ISSUE."
  (let-alist issue
    (save-match-data
      (when (string-match (concat (rx bos)
                                  (regexp-quote ghub-base-url)
                                  (rx "/repos/"
                                      (group (+ (not (any "/")))) "/"
                                      (group (+ (not (any "/")))) "/issues/")
                                  (regexp-quote (number-to-string .number))
                                  (rx eos))
                          .url)
        (magithub-repo
         `((owner (login . ,(match-string 1 .url)))
           (name . ,(match-string 2 .url))))))))

(defun magithub-issue-insert-sections (issues)
  "Insert ISSUES into the buffer with alignment.
See also `magithub-issue-insert-section'."
  (let ((max-num-len (thread-last issues
                       (ghubp-get-in-all '(number))
                       (apply #'max)
                       (number-to-string)
                       (length))))
    (--map (magithub-issue-insert-section it max-num-len)
           issues)))

(defun magithub-issue-insert-section (issue &optional pad-num-to-len)
  "Insert ISSUE into the buffer.
If PAD-NUM-TO-LEN is non-nil, it is an integer width.  For
example, if this section's issue number is \"3\" and the next
section's number is \"401\", pass a padding of 3 to both to align
them.

See also `magithub-issue-insert-sections'."
  (when issue
    (setq pad-num-to-len (or pad-num-to-len 0))
    (magit-insert-section (magithub-issue issue t)
      (let-alist issue
        (magit-insert-heading
          (format (format "%%%ds  %%s" (1+ pad-num-to-len)) ;1+ accounts for #
                  (propertize (format "#%d" .number) 'face 'magithub-issue-number)
                  (propertize .title                 'face 'magithub-issue-title)))
        (run-hook-with-args 'magithub-issue-details-hook issue
                            (format " %s  %%-12s" (make-string pad-num-to-len ?\ )))))))

(defvar magithub-issue-details-hook
  '(magithub-issue-detail-insert-created
    magithub-issue-detail-insert-updated
    magithub-issue-detail-insert-author
    magithub-issue-detail-insert-assignees
    magithub-issue-detail-insert-labels
    magithub-issue-detail-insert-body-preview)
  "Detail functions for issue-type sections.
These details appear under issues as expandable content.

Each function takes two arguments:

 1. an issue object
 2. a format string for a string label (for alignment)")

(defun magithub-issue-detail-insert-author (issue fmt)
  "Insert the author of ISSUE using FMT."
  (let-alist issue
    (insert (format fmt "Author:"))
    (magit-insert-section (magithub-user .user)
      (insert
       (propertize .user.login 'face 'magithub-user)))
    (insert "\n")))

(defun magithub-issue-detail-insert-created (issue fmt)
  "Insert when ISSUE was created using FMT."
  (let-alist issue
    (insert (format fmt "Created:")
            (propertize .created_at 'face 'magit-dimmed)
            "\n")))

(defun magithub-issue-detail-insert-updated (issue fmt)
  "Insert when ISSUE was created using FMT."
  (let-alist issue
    (insert (format fmt "Updated:")
            (propertize .updated_at 'face 'magit-dimmed)
            "\n")))

(defun magithub-issue-detail-insert-assignees (issue fmt)
  "Insert the assignees of ISSUE using FMT."
  (let-alist issue
    (insert (format fmt "Assignees:"))
    (if .assignees
        (let ((assignees .assignees) assignee)
          (while (setq assignee (pop assignees))
            (magit-insert-section (magithub-assignee assignee)
              (insert (propertize (alist-get 'login assignee)
                                  'face 'magithub-user)))
            (when assignees
              (insert " "))))
      (magit-insert-section (magithub-assignee)
        (insert (propertize "none" 'face 'magit-dimmed))))
    (insert "\n")))

(defun magithub-issue-detail-insert-body-preview (issue fmt)
  "Insert a preview of ISSUE's body using FMT."
  (let-alist issue
    (let* ((label-string (format fmt "Preview:"))
           (label-len (length label-string))
           (prefix (make-string label-len ?\ ))
           did-cut)
      (insert label-string
              (if (or (null .body) (string= .body ""))
                  (concat (propertize "none" 'face 'magit-dimmed))
                (concat
                 (s-trim
                  (with-temp-buffer
                    (insert (s-word-wrap (- fill-column label-len) .body))
                    (goto-char 0)
                    (let ((lines 0))
                      (while (and (not (eobp))
                                  (< (setq lines (1+ lines)) 4))
                        (insert prefix)
                        (forward-line))
                      (unless (= (point) (point-max))
                        (delete-region (point) (point-max))
                        (setq did-cut t)))
                    (replace-regexp-in-string
                     "" "" (buffer-string))))
                 (if did-cut "...")))
              "\n"))))

(defun magithub-issue-detail-insert-labels (issue fmt)
  "Insert ISSUE's labels using FMT."
  (let-alist issue
    (insert (format fmt "Labels:"))
    (if-let ((labels .labels) (l t))
        (while (setq l (pop labels))
          (magit-insert-section (magithub-label l)
            (insert (magithub-label-propertize l)))
          (when labels
            (insert " ")))
      (magit-insert-section (magithub-label)
        (insert (propertize "none" 'face 'magit-dimmed))))
    (insert "\n")))

(provide 'magithub-issue)
;;; magithub-issue.el ends here
