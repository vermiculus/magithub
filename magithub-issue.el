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
(require 'markdown-mode)

(require 'magithub-core)
(require 'magithub-cache)
(require 'magithub-proxy)

(magit-define-popup magithub-issues-popup
  "Popup console for creating GitHub issues."
  'magithub-commands
  :man-page "hub"
  :options '((?l "Add labels" "--label=" magithub-issue-read-labels))
  :actions '((?c "Create new issue" magithub-issue-new)))

(define-derived-mode magithub-issue-mode gfm-mode
  "Magithub Issue"
  "Major-mode for creating issues and pull requests for GitHub"
  :group 'magithub
  (use-local-map magithub-issue-mode-map))

(defvar magithub-issue-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c C-c") #'magithub-issue-submit)
    (define-key m (kbd "C-c C-c C-k") #'magithub-issue-cancel)
    (define-key m [remap save-buffer] #'magithub-issue-save-draft)
    m)
  "Keymap used for `magithub-issue-mode'.")

(defconst magithub-dir
  (expand-file-name "magithub" user-emacs-directory)
  "Data directory.")

(defun magithub-issue-dir (owner repo)
  "Data directory for OWNER/REPO."
  (expand-file-name (format "%s/%s/issues" owner repo)
                    magithub-dir))

(defun magithub-issue-new-filename (owner repo)
  "Determine an appropriate filename an issue draft on OWNER/REPO."
  (let ((name ""))
    (while (file-exists-p name)
      (setq name (expand-file-name
                  (with-temp-buffer (uuidgen nil) (buffer-string))
                  (magithub-issue-dir owner repo))))
    name))

(defun magithub-issue-save-draft ()
  "Saves an issue to disk.

Right now, there is no way to automatically resume this draft,
though this feature is planned."
  (interactive)
  (let ((owner (plist-get magithub-issue--info :owner))
        (repo (plist-get magithub-issue--info :repo)))
    (save-excursion
      (goto-char 0)
      (insert (format "Owner: %s\nRepository: %s\n" owner repo)))
    (write-region (point-min) (point-max)
                  (magithub-issue-new-filename owner repo)
                  nil nil nil 'excl))
  (message "Issue saved"))

(defvar-local magithub-issue--info nil
  "Buffer-local variable for issue info")

(defun magithub-issue-new (owner repo title labels)
  "Create a new issue.

OWNER and REPO identifies the target repository for the new
issue.  Interactively, this is determined by
`magithub-source-repo'.

TITLE is the title of the new issue.

LABELS is a list of string labels to give to the new issue.  Note
that these are ignored when creating issues on repositories where
you do not have push access."
  (interactive (append (magithub-source-repo t)
                       (list
                        (read-string "Issue title: ")
                        (magithub-issue-read-labels-list "Issue labels: "))))
  (with-current-buffer (generate-new-buffer "*magithub issue*")
    (magithub-issue-mode)
    (insert (format "Title: %s\nLabels: %s\n\n"
                    title (s-join "," labels)))
    (magithub-with-current-repo owner repo
      (setq magithub-issue--info
            (list :owner owner :repo repo
                  :title title :labels labels)))
    (pop-to-buffer (current-buffer))))

(defun magithub-issue-submit (&rest repo-info)
  "Submit a new issue to this repository.

REPO-INFO should be a plist with :owner and :repo string
properties.  The default is `magithub-issue--info' (set in
`magithub-issue-new')."
  (interactive magithub-issue--info)
  (unless repo-info
    (magithub-error "no issue" "No issue information available."))
  (let ((new-issue
         (magithub-issue--parse-new-issue
          (buffer-substring-no-properties
           (point-min) (point-max))))
        (owner (plist-get repo-info :owner))
        (repo (plist-get repo-info :repo))
        response)
    (when (yes-or-no-p (format "Submit this new issue to %s/%s? " owner repo))
      (setq response (gh-issues-issue-new
                      (gh-issues-api "api")
                      owner repo new-issue))
      (if (not response)
          (magithub-error "issue submission failed"
                          "Failed to submit new issue.")
        (kill-buffer-and-window)
        (when (y-or-n-p (format "#%d submitted; open in your browser? " (oref* response data :number)))
          (magithub-issue-browse (oref response data)))))))

;;; todo: make headers read-only
(defun magithub-issue--parse-new-issue (text)
  "Parse TEXT and return a gh-issues-issue object.

TEXT is assumed to be of the following format:

    Title: My issue title
    Labels: bug,enhancement,...

    Start of issue body."
  (let* ((parts (s-split-up-to "\n\n" text 1))
         (header (mapcar (lambda (s) (apply #'cons (s-split-up-to ": " s 1)))
                         (s-split "\n" (car parts))))
         (body (cadr parts)))
    (make-instance 'gh-issues-issue
                   :title (cdr (assoc "Title" header))
                   :labels (mapcar (lambda (n) (make-instance 'gh-issues-label :name (s-trim n)))
                                   (s-split "," (cdr (assoc "Labels" header))))
                   :body body)))

(defun magithub--issue-list ()
  "Return a list of issues for the current repository."
  (with-temp-message "Retrieving issue list..."
    (magithub-with-current-repo user repo
      (gh-issues-issue-list (gh-issues-api "API") user repo))))

(defun magithub-issue-read-labels (prompt &optional default)
  "Read some issue labels and return a comma-separated string.
Available issues are provided by `magithub-issue-label-list'.

DEFAULT is a comma-separated list of issues -- those issues that
are in DEFAULT are not prompted for again."
  (->> (when default (s-split "," default t))
       (magithub-issue-read-labels-list prompt)
       (s-join ",")))

(defun magithub-issue-read-labels-list (prompt &optional default)
  "Read some issue labels and return a list of strings.
Available issues are provided by `magithub-issue-label-list'.

DEFAULT is a list of pre-selected labels.  These labels are not
prompted for again."
  (magithub--completing-read-multiple
   (format "%s... %s" prompt "Issue labels (or \"\" to quit): ")
   (cl-set-difference (magithub-issue-label-list) default)))

(defun magithub-issue-label-list ()
  "Return a list of issue labels."
  (magithub-with-current-repo user repo
    (mapcar (lambda (label-obj) (oref label-obj :name))
            (oref (gh-issues-label-list (gh-issues-api "API") user repo)
                  data))))

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
