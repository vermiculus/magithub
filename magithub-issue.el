;;; magithub-issue.el --- Browse issues with Magithub  -*- lexical-binding: t; -*-

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

;; Jump to issues from `magit-status'!

;;; Code:

(require 'magit)
(require 'magit-section)
(require 'dash)
(require 's)

(require 'magithub-core)
(require 'magithub-cache)

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

(defun magithub-issue--sort (issues)
  "Sort ISSUES by issue number."
  (sort issues
        (lambda (a b) (< (plist-get a :number)
                         (plist-get b :number)))))

(defun magithub-issue--url-type (url)
  "If URL corresponds to an issue, the symbol `issue'.
If URL correponds to a pull request, the symbol `pull-request'."
  (if (string-match-p (rx "/pull/" (+ digit) eos) url)
      'pull-request 'issue))

(defun magithub-issue--process-line-2.2.8 (s)
  "Process a line S into an issue.

Returns a plist with the following properties:

  :number  issue or pull request number
  :type    either 'pull-request or 'issue
  :title   the title of the issue or pull request
  :url     link to issue or pull request"
  (let (number title url)
    (if (ignore-errors
          (with-temp-buffer
            (insert s)
            (goto-char 0)
            (search-forward "]")
            (setq number (string-to-number (substring s 0 (point))))
            (setq title (substring s (point)
                                   (save-excursion
                                     (goto-char (point-max))
                                     (- (search-backward "(") 2))))
            (goto-char (point-max))
            (delete-char -2)
            (search-backward "(")
            (forward-char 2)
            (setq url (buffer-substring-no-properties (point) (point-max)))
            t))
        (list :number number
              :type (magithub-issue--url-type url)
              :title title
              :url url)
      (magithub-error
       "failed to parse issue"
       "There was an error parsing issues."))))

(defun magithub--issue-list--internal-2.2.8 ()
  "Backwards compatibility for old versions of hub.
See `magithub-issue-list--internal'."
  (magithub-issue--sort
   (mapcar #'magithub-issue--process-line-2.2.8
           (magithub--command-output "issue"))))

(defun magithub-issue--process-line (s)
  "Process a line S into an issue.

Returns a plist with the following properties:

  :number  issue or pull request number
  :type    either 'pull-request or 'issue
  :title   the title of the issue or pull request
  :url     link to issue or pull request"
  (let ((ss (split-string s ",")))
    (list
     :number (string-to-number (car ss))
     :url (cadr ss)
     :title (s-join "," (cddr ss))
     :type (magithub-issue--url-type (cadr ss)))))

(defun magithub--issue-list--internal ()
  "Return a new list of issues for the current repository."
  (magithub-issue--sort
   (mapcar #'magithub-issue--process-line
           (magithub--command-output "issue" '("--format=%I,%U,%t%n")))))

(defun magithub--issue-list ()
  "Return a list of issues for the current repository."
  (magithub-cache (magithub-repo-id) :issues
    '(with-temp-message "Retrieving issue list..."
       (if (magithub-hub-version-at-least "2.3")
           (magithub--issue-list--internal)
         (magithub--issue-list--internal-2.2.8)))))

(defun magithub-issue--wrap-title (title indent)
  "Word-wrap string TITLE to `fill-column' with an INDENT."
  (s-replace
   "\n" (concat "\n" (make-string indent ?\ ))
   (s-word-wrap (- fill-column indent) title)))

(defun magithub-issue--insert (issue)
  "Insert an `issue' as a Magit section into the buffer."
  (when issue
    (magit-insert-section (magithub-issue issue)
      (insert (format " %3d  %s\n"
                      (plist-get issue :number)
                      (magithub-issue--wrap-title
                       (plist-get issue :title) 6))))))

(defun magithub-issue-browse (issue)
  "Visits `issue' in the browser.
Interactively, this finds the issue at point.

If `issue' is nil, open the repository's issues page."
  (interactive (list (magit-section-value
                      (magit-current-section))))
  (browse-url
   (if (plist-member issue :url)
       (plist-get issue :url)
     (car (magithub--command-output "browse" '("--url-only" "--" "issues"))))))

(defun magithub-issue-refresh ()
  "Refresh issues for this repository."
  (interactive)
  (magithub-cache-clear (magithub-repo-id) :issues)
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

(defun magithub--issues-of-type (type)
  "Filter `magithub--issue-list' for issues of type TYPE."
  (-filter (lambda (i) (eq (plist-get i :type) type))
           (magithub--issue-list)))

(defun magithub-issues ()
  "Return a list of issue objects that are actually issues."
  (magithub--issues-of-type 'issue))

(defun magithub-pull-requests ()
  "Return a list of issue objects that are actually pull requests."
  (magithub--issues-of-type 'pull-request))

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
