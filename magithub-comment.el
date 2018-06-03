;;; magithub-comment.el --- tools for comments   -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Sean Allred

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
(require 'markdown-mode)
(require 'thingatpt)

(require 'magithub-core)
(require 'magithub-repo)
(require 'magithub-issue)
(require 'magithub-edit-mode)

(declare-function magithub-issue-view "magithub-issue-view.el" (issue))

(defvar magit-magithub-comment-section-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m magithub-map)
    (define-key m [remap magithub-browse-thing] #'magithub-comment-browse)
    (define-key m [remap magit-delete-thing] #'magithub-comment-delete)
    (define-key m (kbd "SPC") #'magithub-comment-view)
    (define-key m [remap magithub-reply-thing] #'magithub-comment-reply)
    (define-key m [remap magithub-edit-thing] #'magithub-comment-edit)
    m))

(defun magithub-comment-browse (comment)
  (interactive (list (thing-at-point 'github-comment)))
  (unless comment
    (user-error "No comment found"))
  (let-alist comment
    (browse-url .html_url)))

(declare-function face-remap-remove-relative "face-remap.el" (cookie))
(defun magithub-comment-delete (comment)
  (interactive (list (thing-at-point 'github-comment)))
  (unless comment
    (user-error "No comment found"))
  (let ((repo (magithub-comment-source-repo comment))
        (author (let-alist comment .user.login))
        (me (let-alist (magithub-user-me) .login)))
    (unless (or (string= author me)
                (magithub-repo-admin-p repo))
      (user-error "You don't have permission to delete this comment"))
    (let ((cookie (face-remap-add-relative 'magit-section-highlight
                                           ;;'magit-diff-removed-highlight
                                           ;;:strike-through t
                                           ;;:background "red4"
                                           ;;
                                           'magithub-deleted-thing
                                           )))
      (unwind-protect (magithub-confirm 'comment-delete)
        (face-remap-remove-relative cookie)))
    (magithub-request
     (ghubp-delete-repos-owner-repo-issues-comments-id repo comment))
    (magithub-cache-without-cache :issues
      (magit-refresh-buffer))
    (message "Comment deleted")))

(defun magithub-comment-source-issue (comment)
  (magithub-cache :comment
    `(magithub-request
      (ghubp-follow-get ,(alist-get 'issue_url comment)))))

(defun magithub-comment-source-repo (comment)
  (magithub-issue-repo (magithub-comment-source-issue comment)))

(defun magithub-comment-insert (comment)
  "Insert a single issue COMMENT."
  (let-alist comment
    (magit-insert-section (magithub-comment comment)
      (magit-insert-heading (propertize .user.login 'face 'magithub-user))
      (save-excursion
        (let ((created-at (magithub--format-time .created_at)))
          (backward-char 1)
          (insert (make-string (- (current-fill-column)
                                  (current-column)
                                  (length created-at))
                               ? ))
          (insert (propertize created-at 'face 'magit-dimmed))))
      (insert (magithub-fill-gfm (magithub-wash-gfm (s-trim .body)))
              "\n\n"))))

(defvar magithub-gfm-view-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap kill-this-buffer] #'magithub-comment-view-close)
    m)
  "Keymap for `magithub-gfm-view-mode'.")

(declare-function gfm-view-mode "ext:markdown-mode.el")
(define-derived-mode magithub-gfm-view-mode gfm-view-mode "M:GFM-View"
  "Major mode for viewing GitHub markdown content.")

(defvar-local magithub-comment-view--parent-buffer nil
  "The 'parent' buffer of the current comment-view.
This variable is used to jump back to the issue that contained
the comment; see `magithub-comment-view' and
`magithub-comment-view-close'.")

(defun magithub-comment-view (comment)
  "View COMMENT in a new buffer."
  (interactive (list (thing-at-point 'github-comment)))
  (let ((prev (current-buffer)))
    (with-current-buffer (get-buffer-create "*comment*")
      (magithub-gfm-view-mode)
      (setq-local magithub-comment-view--parent-buffer prev)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (magithub-wash-gfm (alist-get 'body comment))))
      (goto-char 0)
      (magit-display-buffer (current-buffer)))))

(defun magithub-comment-view-close ()
  "Close the current buffer."
  (interactive)
  (let ((oldbuf magithub-comment-view--parent-buffer))
    (kill-this-buffer)
    (magit-display-buffer oldbuf)))

;;;###autoload
(defun magithub-comment-new (issue &optional discard-draft initial-content)
  "Comment on ISSUE in a new buffer.
If prefix argument DISCARD-DRAFT is specified, the draft will not
be considered.

If INITIAL-CONTENT is specified, it will be inserted as the
initial contents of the reply if there is no draft."
  (interactive (let ((issue (magithub-interactive-issue)))
                 (prog1 (list issue current-prefix-arg)
                   (unless (derived-mode-p 'magithub-issue-view-mode)
                     (magithub-issue-view issue)))))
  (let* ((issueref (magithub-issue-reference issue))
         (repo (magithub-issue-repo issue)))
    (with-current-buffer
        (magithub-edit-new (concat "reply to " issueref)
          :header (concat "replying to " issueref)
          :submit #'magithub-issue-comment-submit
          :content initial-content
          :prompt-discard-draft discard-draft
          :file (magithub-comment--draft-file issue repo))
      (setq-local magithub-issue issue)
      (setq-local magithub-repo repo)
      (magit-display-buffer (current-buffer)))))

(defun magithub-comment--draft-file (issue repo)
  "Get an appropriate draft file for ISSUE in REPO."
  (let-alist issue
    (expand-file-name (format "%s-comment" .number)
                      (magithub-repo-data-dir repo))))

(defun magithub-comment--submit-edit (comment repo new-body)
  (interactive (list (thing-at-point 'github-comment)
                     (thing-at-point 'github-repository)
                     (buffer-string)))
  (when (string= new-body "")
    (user-error "Can't post an empty comment; try deleting it instead"))
  (magithub-confirm 'comment-edit)
  (magithub-request
   (ghubp-patch-repos-owner-repo-issues-comments-id
       repo comment
       `((body . ,new-body)))))

(defun magithub-comment-edit (comment issue repo)
  "Edit COMMENT."
  (interactive (list (thing-at-point 'github-comment)
                     (or (thing-at-point 'github-issue)
                         (thing-at-point 'github-pull-request))
                     (thing-at-point 'github-repository)))
  (let ((updated (magithub-request (ghubp-follow-get (alist-get 'url comment)))))
    (with-current-buffer
        (magithub-edit-new (format "*%s: editing comment by %s (%s)*"
                                   (magithub-issue-reference issue)
                                   (let-alist comment .user.login)
                                   (alist-get 'id comment))
          :submit #'magithub-comment--submit-edit
          :content (alist-get 'body updated)
          :file (magithub-comment--draft-file issue repo))
      (setq-local magithub-issue issue)
      (setq-local magithub-repo repo)
      (setq-local magithub-comment updated)
      (magit-display-buffer (current-buffer)))

    (unless (string= (alist-get 'body comment)
                     (alist-get 'body updated))
      (message "Comment has changed since information was cached; \
updated content pulled in for edit"))))

(defun magithub-comment-reply (comment &optional discard-draft issue)
  "Reply to COMMENT on ISSUE.
If prefix argument DISCARD-DRAFT is provided, the current draft
will deleted.

If ISSUE is not provided, it will be determined from context or
from COMMENT."
  (interactive (list (thing-at-point 'github-comment)
                     current-prefix-arg
                     (thing-at-point 'github-issue)))
  (let-alist comment
    (magithub-comment-new
     (or issue (magithub-request (ghubp-follow-get .issue_url)))
     discard-draft
     (let ((reply-body (if (use-region-p)
                           (buffer-substring (region-beginning) (region-end))
                           .body)))
      (with-temp-buffer
        (insert (string-trim (magithub-wash-gfm reply-body)))
        (markdown-blockquote-region (point-min) (point-max))
        (goto-char (point-max))
        (insert "\n\n")
        (buffer-string))))))

(defun magithub-issue-comment-submit (issue comment &optional repo)
  "On ISSUE, submit a new COMMENT.

COMMENT is the text of the new comment.

REPO is an optional repo object; it will be deduced from ISSUE if
not provided."
  (interactive (list (thing-at-point 'github-issue)
                     (save-restriction
                       (widen)
                       (buffer-substring-no-properties (point-min) (point-max)))
                     (thing-at-point 'github-repository)))
  (unless issue
    (user-error "No issue provided"))
  (setq repo (or repo
                 (magithub-issue-repo issue)
                 (thing-at-point 'github-repository)))
  (unless repo
    (user-error "No repo detected"))
  ;; all required args provided
  (magithub-confirm 'comment (magithub-issue-reference issue))
  (magithub-request
   (ghubp-post-repos-owner-repo-issues-number-comments
       repo issue `((body . ,comment))))
  (magithub-edit-delete-draft)
  (message "Success"))

(provide 'magithub-comment)
;;; magithub-comment.el ends here
