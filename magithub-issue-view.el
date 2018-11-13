;;; magithub-issue-view.el --- view issues       -*- lexical-binding: t; -*-

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

;; View issues in magit-like buffers.

;;; Code:

(require 'magit-mode)

(require 'magithub-core)
(require 'magithub-issue)
(require 'magithub-comment)

(defvar magithub-issue-view-mode-map
  (let ((m (make-composed-keymap (list magithub-map) magit-mode-map)))
    (define-key m [remap magithub-reply-thing] #'magithub-comment-new)
    (define-key m [remap magithub-browse-thing] #'magithub-issue-browse)
    (define-key m [remap magit-refresh] #'magithub-issue-view-refresh)
    (define-key m [remap magithub-edit-thing] #'magithub-issue-body-edit)
    m))

(define-derived-mode magithub-issue-view-mode magit-mode
  "Issue View" "View GitHub issues with Magithub.")

(defvar magithub-issue-view-headers-hook
  '(magithub-issue-view-insert-title
    magithub-issue-view-insert-author
    magithub-issue-view-insert-state
    magithub-issue-view-insert-asked
    magithub-issue-view-insert-labels)
  "Header information for each issue.")

(defvar magithub-issue-view-sections-hook
  '(magithub-issue-view-insert-headers
    magithub-issue-view-insert-body
    magithub-issue-view-insert-comments)
  "Sections to be inserted for each issue.")

(defun magithub-issue-view-refresh ()
  "Refresh the current issue."
  (interactive)
  (if (derived-mode-p 'magithub-issue-view-mode)
      (progn
        ;; todo: find a better means to separate the keymaps of issues
        ;; in the status buffer vs issues in their own buffer
        (when magithub-issue
          (magithub-cache-without-cache :issues
            (setq-local magithub-issue
                        (magithub-issue magithub-repo magithub-issue))
            (magithub-issue-comments magithub-issue)))
        (let ((magit-refresh-args (list magithub-issue)))
          (magit-refresh))
        (message "refreshed"))
    (call-interactively #'magit-refresh)))

(defun magithub-issue-view-refresh-buffer (issue &rest _)
  (setq-local magithub-issue issue)
  (setq-local magithub-repo (magithub-issue-repo issue))
  (magit-insert-section (magithub-issue issue)
    (run-hooks 'magithub-issue-view-sections-hook)))

(defun magithub-issue-view-insert-headers ()
  "Run `magithub-issue-view-headers-hook'."
  (magit-insert-headers 'magithub-issue-view-headers-hook))

(defun magithub-issue-view--lock-value (args)
  "Provide an identifying value for ISSUE.
See also `magit-buffer-lock-functions'."
  (let ((issue (car args)))
    (let-alist `((repo . ,(magithub-issue-repo issue))
                 (issue . ,issue))
      (list .repo.owner.login .repo.name .issue.number))))
(push (cons 'magithub-issue-view-mode #'magithub-issue-view--lock-value)
      magit-buffer-lock-functions)

(defun magithub-issue-view--buffer-name (_mode issue-lock-value)
  "Generate a buffer name for ISSUE-LOCK-VALUE.
See also `magithub-issue-view--lock-value'."
  (let ((owner  (nth 0 issue-lock-value))
        (repo   (nth 1 issue-lock-value))
        (number (nth 2 issue-lock-value)))
    (format "*magithub: %s/%s#%d: %s*"
            owner
            repo
            number
            (alist-get 'title (magithub-issue `((owner (login . ,owner))
                                                (name . ,repo))
                                              number)))))

;;;###autoload
(defun magithub-issue-view (issue)
  "View ISSUE in a new buffer.
Return the new buffer."
  (interactive (list (magithub-interactive-issue)))
  (let ((magit-generate-buffer-name-function #'magithub-issue-view--buffer-name))
    (magit-mode-setup-internal #'magithub-issue-view-mode (list issue) t)
    (current-buffer)))

(cl-defun magithub-issue-view-insert--generic (title text &optional type section-value &key face)
  "Insert a generic header line with TITLE: VALUE"
  (declare (indent 2))
  (setq type (or type 'magithub))
  (magit-insert-section ((eval type) section-value)
    (insert (format "%-10s" title)
            (or (and face (propertize text 'face face))
                text)
            ?\n)
    (magit-insert-heading)))

(defun magithub-issue-view-insert-title ()
  "Insert issue title."
  (let-alist magithub-issue
    (magithub-issue-view-insert--generic "Title:" .title)))

(defun magithub-issue-view-insert-author ()
  "Insert issue author."
  (insert (format "%-10s" "Author:"))
  (let-alist magithub-issue
    (magit-insert-section (magithub-user .user)
      (insert (propertize .user.login 'face 'magithub-user) ?\n)
      (magit-insert-heading))))

(defun magithub-issue-view-insert-state ()
  "Insert issue state."
  (magithub-issue-view-insert--generic "State:"
      (if (magithub-issue-open-p magithub-issue)
          (propertize "Open" 'face 'magithub-issue-open)
        (propertize "Closed" 'face 'magithub-issue-closed))
    :face 'magit-dimmed))

(defun magithub-issue-view-insert-asked ()
  "Insert posted time."
  (let-alist magithub-issue
    (magithub-issue-view-insert--generic "Posted:" (magithub--format-time .created_at)
      :face 'magit-dimmed)))

(defun magithub-issue-view-insert-labels ()
  "Insert labels."
  (insert (format "%-10s" "Labels:"))
  (magithub-label-insert-list (alist-get 'labels magithub-issue))
  (insert ?\n))

(defun magithub-issue-view-insert-body ()
  "Insert issue body."
  (let-alist magithub-issue
    (magit-insert-section (magithub-issue-body magithub-issue)
      (magit-insert-heading "Body")
      (if (or (null .body) (string= .body ""))
          (insert (propertize "There's nothing here!\n\n" 'face 'magit-dimmed))
        (insert (magithub-fill-gfm (magithub-wash-gfm (s-trim .body))) "\n\n")))))

(defun magithub-issue-view-insert-comments ()
  "Insert issue comments."
  (let ((comments (magithub-issue-comments magithub-issue)))
    (magit-insert-section (magithub-issue-comments comments)
      (magit-insert-heading "Comments:")
      (if (null comments)
          (insert (propertize "There's nothing here!\n\n" 'face 'magit-dimmed))
        (mapc #'magithub-comment-insert comments)))))

(defun magithub-issue--submit-edit (issue repo new-body)
  (interactive (list (thing-at-point 'github-issue)
                     (thing-at-point 'github-repository)
                     (buffer-string)))
  (let ((issue-content (magithub-issue-post--parse-buffer new-body)))
    (when (s-blank-p (alist-get 'title issue-content))
      (user-error "Title is required"))
    (magithub-confirm 'issue-edit
                      (magithub-issue-reference issue))
    (magithub-request
     (ghubp-patch-repos-owner-repo-issues-number
         repo issue issue-content))))

(defun magithub-issue-body-edit (issue repo)
  "Edit body of ISSUE."
  (interactive (list (or (thing-at-point 'github-issue)
                         (thing-at-point 'github-pull-request))
                     (thing-at-point 'github-repository)))
  (let* ((updated (magithub-request (ghubp-follow-get (alist-get 'url issue))))
         (content (concat
                   (alist-get 'title updated)
                   "\n\n"
                   (replace-regexp-in-string "" "" (alist-get 'body updated)))))
    (with-current-buffer
        (magithub-edit-new (format "*%s: editing issue by %s (%s)*"
                                   (magithub-issue-reference issue)
                                   (let-alist issue .user.login)
                                   (alist-get 'id issue))
          :submit #'magithub-issue--submit-edit
          :content content
          :file (expand-file-name "edit-issue-draft"
                                  (magithub-repo-data-dir repo)))
      (setq-local magithub-issue issue)
      (setq-local magithub-repo repo)
      (setq-local magithub-comment updated)
      (magit-display-buffer (current-buffer)))

    (unless (string= (alist-get 'body issue)
                     (alist-get 'body updated))
      (message "Issue has changed since information was cached; \
updated issue pulled in for edit"))))

(provide 'magithub-issue-view)
;;; magithub-issue-view.el ends here
