;;; magithub-pr.el --- pull request utilities        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: hypermedia, extensions, tools

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

;; Work with PRs semi-transparently as branchs

;;; Code:

(require 'magithub-core)
(require 'magithub-issue)

(defun magithub-pull-request (repo number)
  "Retrieve a pull request in REPO by NUMBER."
  (magithub-cache :issues
    `(ghubp-get-repos-owner-repo-pulls-number
      ',repo '((number . ,number)))
    (format "Getting pull request %s#%d..."
            (magithub-repo-name repo)
            number)))

(defun magithub-remote-fork-p (remote)
  "True if REMOTE is a fork."
  (thread-last remote
    (magithub-repo-from-remote)
    (alist-get 'fork)))

(defun magithub-pull-request-checked-out (pull-request)
  "True if PULL-REQUEST is currently checked out."
  (let-alist pull-request
    (let ((remote .user.login)
          (branch .head.ref))
      (and (magit-remote-p remote)
           (magithub-remote-fork-p remote)
           (magit-branch-p branch)
           (string= remote (magit-get-push-remote branch))))))

(defun magithub-pull-request-checkout (pull-request)
  "Checkout PULL-REQUEST.
PULL-REQUEST is the full object; not just the issue subset."
  (interactive (list
                (let ((pr (or (magithub-thing-at-point 'pull-request)
                              (magithub-issue-completing-read-pull-requests))))
                  (ghubp-get-repos-owner-repo-pulls-number
                   (magithub-repo)
                   `((number . ,(alist-get 'number pr)))))))
  (let-alist pull-request
    (let ((remote .user.login)
          (branch .head.ref))
      (cond
       ((magithub-pull-request-checked-out pull-request)
        (with-temp-message (format "PR#%d is already checked out somewhere; checking out %s"
                                   .number branch)
          (magit-checkout branch)
          (magit-fetch remote (magit-fetch-arguments))))
       ((magit-branch-p branch)
        (user-error "Cannot checkout pull request: branch `%s' already exists; rename branch on remote" branch))
       (t
        (magithub--run-git-synchronously
         ;; get remote
         (unless (magit-remote-p remote)
           (magit-remote-add remote (magithub-repo--clone-url .head.repo)))
         (magit-fetch remote (magit-fetch-arguments))
         ;; create branch
         (magit-git-success "branch" branch .base.sha)  ; also sets upstream to base ref
         ;; set push to remote branch
         (magit-set (concat "refs/heads/" .base.ref) "branch" branch "merge")
         (magit-set "." "branch" branch "remote") ;same as merge
         (magit-set remote "branch" branch "pushRemote")
         (magit-set (number-to-string .number) "branch" branch "magithub" "sourcePR")
         ;; set descripiton
         (magit-set (concat "PR: " .title) "branch" branch "description")
         ;; Checkout
         (magit-git-success "checkout" branch)
         (magit-refresh)))))))

(provide 'magithub-pr)
;;; magithub-pr.el ends here
