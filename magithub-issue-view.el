;;; magithub-issue-view.el --- view issues           -*- lexical-binding: t; -*-

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
    (define-key m [remap magit-merge-popup] #'magithub-pull-request-merge-online)
    m))

(define-derived-mode magithub-issue-view-mode magit-mode
  "Issue View" "View Github issues with Magithub.")

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
  (magit-insert-headers magithub-issue-view-headers-hook))

(defun magithub-issue-view--lock-value (issue &rest _args)
  "Provide an identifying value for ISSUE.
See also `magit-buffer-lock-functions'."
  (let-alist `((repo . ,(magithub-issue-repo issue))
               (issue . ,issue))
    (list .repo.owner.login .repo.name .issue.number)))
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
  "View ISSUE in a new buffer."
  (interactive (list (magithub-interactive-issue)))
  (let ((magit-generate-buffer-name-function #'magithub-issue-view--buffer-name))
    (magit-mode-setup-internal #'magithub-issue-view-mode (list issue) t)))

(cl-defun magithub-issue-view-insert--generic (title text &optional type section-value &key face)
  "Insert a generic header line with TITLE: VALUE"
  (declare (indent 2))
  (setq type (or type 'magithub))
  (magit-insert-section ((eval type) section-value)
    (insert (format "%-10s" title)
            (or (and face (propertize text 'face face))
                text))
    (magit-insert-heading)))

(defun magithub-issue-view-insert-title ()
  "Insert issue title."
  (let-alist magithub-issue
    (magithub-issue-view-insert--generic "Title:" .title)))

(defun magithub-issue-view-insert-author ()
  "Insert issue author."
  (let-alist magithub-issue
    (magithub-issue-view-insert--generic "Author:" .user.login
      'magithub-user .user
      :face 'magithub-user)))

(defun magithub-issue-view-insert-state ()
  "Insert issue state (either \"open\" or \"closed\")."
  (let-alist magithub-issue
    (magithub-issue-view-insert--generic "State:" .state
      :face 'magit-dimmed)))

(defun magithub-issue-view-insert-asked ()
  "Insert posted time."
  (let-alist magithub-issue
    (magithub-issue-view-insert--generic "Posted:" (magithub--format-time .created_at)
      :face 'magit-dimmed)))

(defun magithub-issue-view-insert-labels ()
  "Insert labels."
  (magit-insert-section (magithub-label)
    (insert (format "%-10s" "Labels:"))
    (magithub-label-insert-list (alist-get 'labels magithub-issue))
    (magit-insert-heading)))

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

(defun magithub-pull-request-merge-online (pull-request &optional merge-method repo)
  (interactive (let* ((pr (magithub-thing-at-point 'issue))
                      (repo (magithub-issue-repo pr)))
                 (unless (magithub-repo-push-p repo)
                   (user-error "You don't have permission to merge pull requests in this repository"))
                 (unless (magithub-issue--issue-is-pull-p pr)
                   (error "Not a pull request: %S" pr))
                 (list pr nil repo)))

  (let-alist pull-request
    ;; if it's an issue object, convert to a PR
    ;; if it's already a PR, make sure we have the most up-to-date version
    (setq pull-request (ghubp-follow-get .pull_request.url)))

  (let-alist pull-request
    (when .merged
      (user-error "This pull request has already been merged"))
    (unless .mergeable
      (if (y-or-n-p "This pull request cannot be merged; open in browser? ")
          (magithub-issue-browse pull-request)
        (user-error "This pull request cannot be merged")))
    (if (and (string= .mergeable_state "behind")
             (string= .author_association "CONTRIBUTOR")
             .maintainer_can_modify)
        ;; head is behind base
        (if (y-or-n-p (format "%s is behind; do you want to update it (opens browser)? " .head.label))
            ;; the following will fail; I've got a GitHub API support ticket in
            ;; merge base into head
            ;; (let ((base .head.label)
            ;;       (head .base.sha))
            ;;   (condition-case err
            ;;       (ghub-post (format "/repos/%s/merges" (magithub-repo-name repo))
            ;;                  `((base . ,head)
            ;;                    (head . ,base)))
            ;;     (ghub-404 (user-error "HTTP 404: %s (base=%s; head=%s) [%S]"
            ;;                           (alist-get 'message (nth 5 err))
            ;;                           base head err))))
            (magithub-issue-browse pull-request)
          (unless (and (magithub-repo-admin-p repo)
                       (y-or-n-p "Do you wish to proceed with the merge anyway? "))
            (user-error "Aborting"))))
    (unless (y-or-n-p (format "Merge %s into %s? " .head.label .base.ref))
      (user-error "Aborting"))

    (setq merge-method
          (or merge-method
              (intern
               (magit-completing-read
                "GitHub merge merge-method"
                '("merge" "squash" "rebase")
                nil t nil nil
                (or (magit-get "magithub" "defaultMergeMethod")
                    "merge")))))

    ;; todo: allow modifying the merge commit
    (condition-case err
        (ghubp-put-repos-owner-repo-pulls-number-merge
         repo pull-request
         ;; todo: use a commit buffer to grab this info
         `((commit_title . ,(format "Merge pull request #%d from %s"
                                    .number .head.label))
           (commit_message . ,(progn .title))
           (sha . ,(progn .head.sha))
           (merge_method . ,(symbol-name merge-method))))
      (ghub-409 (user-error "SHA out-of-date: %S" err)))))

(provide 'magithub-issue-view)
;;; magithub-issue-view.el ends here
