;;; magithub-comment.el --- tools for comments       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: lisp

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

;; Tools for working with issue comments.

;;; Code:

(require 'magit)

(require 'magithub-core)

(defvar magit-magithub-comment-section-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m magithub-map)
    (define-key m [remap magithub-browse-thing] #'magithub-comment-browse)
    m))

(defun magithub-comment-browse (comment)
  (interactive (list (magithub-thing-at-point 'comment)))
  (unless comment
    (user-error "No comment found"))
  (let-alist comment
    (browse-url .html_url)))

(defun magithub-comment-draft-file (repo issue)
  "Get the filepath of the comment draft for REPO/ISSUE."
  (let-alist issue
    (expand-file-name (format "%s-comment" .number)
                      (magithub-repo-data-dir repo))))

(defun magithub-comment-draft-save (repo issue comment)
  "Save a draft reply to REPO/ISSUE as COMMENT."
  (make-directory (magithub-repo-data-dir repo) t)
  (with-temp-buffer
    (insert comment)
    (write-file (magithub-comment-draft-file repo issue)))
  (message "Draft saved"))

(defun magithub-comment-draft-load (repo issue)
  "Load the draft reply to REPO/ISSUE."
  (let ((file (magithub-comment-draft-file repo issue)))
    (or (and (file-exists-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string)))
        "")))

(defun magithub-comment-draft-delete (repo issue)
  "Delete the draft file for REPO/ISSUE if it exists."
  (let ((f (magithub-comment-draft-file repo issue)))
    (when (file-exists-p f)
      (delete-file f magit-delete-by-moving-to-trash))))

(defun magithub-comment-insert (comment)
  "Insert a single issue COMMENT."
  (let-alist comment
    (magit-insert-section (magithub-comment comment)
      (magit-insert-heading (propertize .user.login 'face 'magithub-user))
      (save-excursion
        (backward-char 1)
        (insert (make-string (- (current-fill-column)
                                (current-column)
                                (length .created_at))
                             ? ))
        (insert (propertize .created_at 'face 'magit-dimmed)))
      (insert (magithub-fill-gfm (magithub-wash-gfm (s-trim .body)))
              "\n\n"))))

;;;###autoload
(defun magithub-comment-new (issue)
  "Comment on ISSUE in a new buffer."
  (interactive (let ((issue (magithub-interactive-issue)))
                 (prog1 (list issue)
                   (unless (derived-mode-p 'magithub-issue-view-mode)
                     (magithub-issue-view-issue issue)))))
  (let ((issueref (magithub-issue-reference issue))
        (repo (magithub-issue-repo issue)))
    (magithub-edit-new
        (concat "reply to " issueref)
        #'magithub-issue-comment-submit
        #'magithub-issue-comment-cancel
      (lambda ()
        (setq-local magithub-issue issue)
        (setq-local magithub-repo repo)
        (magit-set-header-line-format
         (substitute-command-keys
          (format "replying to %s | %s | %s"
                  issueref
                  "submit: \\[magithub-edit-commit]"
                  "cancel: \\[magithub-edit-cancel]")))
        (when-let ((draft (magithub-comment-draft-load repo issue)))
          (insert draft)
          (message "Loaded draft"))
        (goto-char (point-min))))))

(defun magithub-issue-comment-cancel (repo issue comment-text)
  "Cancel current comment."
  (interactive (list (magithub-thing-at-point 'repo)
                     (magithub-thing-at-point 'issue)
                     (buffer-string)))
  (magithub-comment-draft-save repo issue comment-text))

(defun magithub-issue-comment-submit (issue comment &optional repo)
  "On ISSUE, submit a new COMMENT.

COMMENT is the text of the new comment.

REPO is an optional repo object; it will be deduced from ISSUE if
not provided."
  (interactive (list (magithub-thing-at-point 'issue)
                     (save-restriction
                       (widen)
                       (buffer-substring-no-properties (point-min) (point-max)))
                     (magithub-thing-at-point 'repo)))
  (unless issue
    (user-error "No issue provided"))
  (setq repo (or repo
                 (magithub-issue-repo issue)
                 (magithub-thing-at-point 'repo)))
  (unless repo
    (user-error "No repo detected"))
  ;; all required args provided
  (unless (y-or-n-p (format "Submit this comment to %s? "
                            (magithub-issue-reference issue)))
    (user-error "Aborted"))
  ;; confirmed; submit the issue
  (ghubp-post-repos-owner-repo-issues-number-comments
   repo issue `((body . ,comment)))
  (message "Success")
  (magithub-cache-without-cache :issues
    (magithub-issue-view-issue issue))
  (magithub-comment-draft-delete repo issue))

(provide 'magithub-comment)
;;; magithub-comment.el ends here
