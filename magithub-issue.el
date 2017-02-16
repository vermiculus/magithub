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

(require 'magit)
(require 'magit-section)
(require 'dash)
(require 's)
(require 'gh)

(require 'magithub-core)
(require 'magithub-cache)
(require 'magithub-proxy)

(magit-define-popup magithub-issues-popup
  "Popup console for creating GitHub issues."
  'magithub-commands
  :man-page "hub"
  :options '((?l "Add labels" "--label=" magithub-issue-read-labels))
  :actions '((?c "Create new issue" magithub-issue-new)))

(defun magithub-issue-new ()
  "Create a new issue on GitHub."
  (interactive)
  (unless (magithub-github-repository-p)
    (user-error "Not a GitHub repository"))
  (magithub--command-with-editor
   "issue" (cons "create" (magithub-issues-arguments))))

(defun magithub--issue-list ()
  "Return a list of issues for the current repository."
  (with-temp-message "Retrieving issue list..."
    (magithub-with-current-repo user repo
      (gh-issues-issue-list (gh-issues-api "API") "vermiculus" "magithub"))))

(defun magithub-issue-label-list ()
  "Return a list of issue labels.
This is a hard-coded list right now."
  (list "bug" "duplicate" "enhancement"
        "help wanted" "invalid" "question" "wontfix"))

(defun magithub-issue-read-labels (prompt &optional default)
  "Read some issue labels and return a comma-separated string.
Available issues are provided by `magithub-issue-label-list'.

DEFAULT is a comma-separated list of issues -- those issues that
are in DEFAULT are not prompted for again."
  ;; todo: probably need to add DEFAULT to the top here
  (s-join
   ","
   (magithub--completing-read-multiple
    (format "%s... %s" prompt "Issue labels (or \"\" to quit): ")
    (let* ((default-labels (when default (s-split "," default t))))
      (cl-set-difference (magithub-issue-label-list) default-labels)))))

(defun magithub-issue-sort-ascending (a b)
  "Lower issue numbers come first."
  (< (plist-get a :number)
     (plist-get b :number)))

(defun magithub-issue-sort-descending (a b)
  "Higher issue numbers come first."
  (< (plist-get b :number)
     (plist-get a :number)))

(defcustom magithub-issue-sort-function
  #'magithub-issue-sort-ascending
  "Function used for sorting issues and pull requests in the
status buffer.  Should take two issue-objects as arguments."
  :type 'function
  :group 'magithub)

(defun magithub-issue--sort (issues)
  "Sort ISSUES by `magithub-issue-sort-function'."
  (sort issues magithub-issue-sort-function))

(defun magithub-issue--wrap-title (title indent)
  "Word-wrap string TITLE to `fill-column' with an INDENT."
  (s-replace
   "\n" (concat "\n" (make-string indent ?\ ))
   (s-word-wrap (- fill-column indent) title)))

(defun magithub-issue--format (issue)
  (with-slots ((number :number) (title :title)) issue
    (format " %4d  %s\n" number (magithub-issue--wrap-title title 7))))

(defun magithub-issue--insert (issue)
  "Insert ISSUE as a Magit section into the buffer."
  (when issue
    (magit-insert-section (magithub-issue issue)
      (insert (magithub-issue--format issue)))))

(defun magithub-issue-browse (issue)
  "Visits `issue' in the browser.
Interactively, this finds the issue at point.

If `issue' is nil, open the repository's issues page."
  (interactive (list (magit-section-value
                      (magit-current-section))))
  (-when-let (url (oref issue :html-url))
    (browse-url url)))

;;; todo: bring back caching
(defun magithub-issue-refresh ()
  "Refresh issues for this repository."
  (interactive)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defvar magit-magithub-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-issue' sections.")

(defvar magit-magithub-issue-list-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-issue-list' sections.")

(defvar magit-magithub-pull-request-list-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-pull-request-list' sections.")

(defun magithub-issue--issue-is-pull-p (issue)
  (oref* issue :pull-request :html-url))

(defun magithub-issue--issue-is-issue-p (issue)
  (not (magithub-issue--issue-is-pull-p issue)))

(defun magithub-issues ()
  "Return a list of issue objects that are actually issues."
  (-filter #'magithub-issue--issue-is-issue-p
           (oref (magithub--issue-list) :data)))

(defun magithub-pull-requests ()
  "Return a list of issue objects that are actually pull requests."
  (-filter #'magithub-issue--issue-is-pull-p
           (oref (magithub--issue-list) :data)))

(defun magithub-issue--insert-issue-section ()
  "Insert GitHub issues if appropriate."
  (when (magithub-usable-p)
    (-when-let (issues (magithub-issues))
      (magit-insert-section (magithub-issue-list)
        (magit-insert-heading "Issues:")
        (mapc #'magithub-issue--insert issues)
        (insert ?\n)))))

(defun magithub-issue--insert-pr-section ()
  "Insert GitHub pull requests if appropriate."
  (magithub-feature-maybe-idle-notify
   'pull-request-merge
   'pull-request-checkout)
  (when (magithub-usable-p)
    (-when-let (pull-requests (magithub-pull-requests))
      (magit-insert-section (magithub-pull-request-list)
        (magit-insert-heading "Pull Requests:")
        (mapc #'magithub-issue--insert pull-requests)
        (insert ?\n)))))

(defun magithub-repolist-column-issue (_id)
  "Insert the number of open issues in this repository."
  (number-to-string (length (magithub-issues))))

(defun magithub-repolist-column-pull-request (_id)
  "Insert the number of open pull requests in this repository."
  (number-to-string (length (magithub-pull-requests))))

(defun magithub-pull-request--format-pr-for-read (pr)
  "Format pull request PR as string suitable for completion."
  (format "%3d %s" (plist-get pr :number) (plist-get pr :title)))

(defun magithub-pull-request--completing-read-list ()
  "Return an alist of PR-strings to full pull-request issue objects.
See `magithub-pull-request--format-pr-for-am'."
  (-when-let (pr-list (magithub-pull-requests))
    (-zip-pair
     (mapcar #'magithub-pull-request--format-pr-for-read pr-list)
     pr-list)))

(defun magithub-pull-request-at-point ()
  "The pull request object at point (or nil)."
  (when (derived-mode-p 'magit-status-mode)
    (-when-let* ((s (magit-current-section))
                 (v (magit-section-value s)))
      (and (listp v)
           (eq (plist-get v :type) 'pull-request)
           v))))

(defun magithub-pull-request--completing-read ()
  "Complete over all open pull requests returning its issue object.
If point is on a pull-request object, that object is selected by
default."
  (let ((prs (magithub-pull-request--completing-read-list)) current-pr)
    (-when-let (tap (magithub-pull-request-at-point))
      (when (and (listp tap) (eq (plist-get tap :type) 'pull-request))
        (setq current-pr (magithub-pull-request--format-pr-for-read tap))))
    (cdr (assoc (completing-read "Pull request: " prs nil t current-pr) prs))))

(defun magithub-pull-request-checkout (pull-request)
  "Checkout PULL-REQUEST as a local branch."
  (interactive (list (magithub-pull-request--completing-read)))
  (-when-let (url (plist-get pull-request :url))
    (magithub-with-hub
     (magit-checkout url))
    (dolist (var-val `(("URL" . ,url)
                       ("ID" . ,(plist-get pull-request :number))))
      (magit-set (cdr var-val)
                 "branch" (magit-get-current-branch)
                 (concat "magithubPullRequest" (car var-val))))))

(defun magithub-pull-request-merge (pull-request &optional args)
  "Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default."
  (interactive (list (magithub-pull-request--completing-read)
                     (magit-am-arguments)))
  (unless (member pull-request (magithub-pull-requests))
    (user-error "Unknown pull request %S" pull-request))
  (magithub-with-hub
   (magit-run-git-sequencer "am" args (plist-get pull-request :url)))
  (message "#%d has been applied" (plist-get pull-request :number)))

;;; Hook into the status buffer
(magithub--deftoggle magithub-toggle-issues
  magit-status-sections-hook #'magithub-issue--insert-issue-section "issues")
(magithub--deftoggle magithub-toggle-pull-requests
  magit-status-sections-hook #'magithub-issue--insert-pr-section "pull requests")

(when (executable-find magithub-hub-executable)
  (magithub-toggle-pull-requests)
  (magithub-toggle-issues))

(provide 'magithub-issue)
;;; magithub-issue.el ends here
