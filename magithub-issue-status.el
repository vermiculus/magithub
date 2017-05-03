(require 's)
(require 'magit)
(require 'magithub-issue)

(magit-define-popup magithub-issues-popup
  "Popup console for managing GitHub issues."
  'magithub-commands
  :options '((?l "Add labels" "--label=" magithub-issue-read-labels))
  :actions '((?c "Create new issue" magithub-issue-new)))

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

(defun magithub-issue--wrap-title (title indent)
  "Word-wrap string TITLE to `fill-column' with an INDENT."
  (s-replace
   "\n" (concat "\n" (make-string indent ?\ ))
   (s-word-wrap (- fill-column indent) title)))

(defun magithub-issue--format (issue)
  (let-alist issue
    (format " %4d  %s\n" .number (magithub-issue--wrap-title .title 7))))

(defun magithub-issue--insert (issue)
  "Insert ISSUE as a Magit section into the buffer."
  (when issue
    (magit-insert-section (magithub-issue issue)
      (insert (magithub-issue--format issue)))))

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

(defun magithub-issue-browse (issue)
  "Visits `issue' in the browser.
Interactively, this finds the issue at point.

If `issue' is nil, open the repository's issues page."
  (interactive (list (magit-section-value
                      (magit-current-section))))
  (-when-let (url (alist-get 'html_url issue))
    (browse-url url)))

(defun magithub-repolist-column-issue (_id)
  "Insert the number of open issues in this repository."
  (number-to-string (length (magithub-issues))))

(defun magithub-repolist-column-pull-request (_id)
  "Insert the number of open pull requests in this repository."
  (number-to-string (length (magithub-pull-requests))))

(magithub--deftoggle magithub-toggle-issues
  magit-status-sections-hook #'magithub-issue--insert-issue-section "issues")
(magithub--deftoggle magithub-toggle-pull-requests
  magit-status-sections-hook #'magithub-issue--insert-pr-section "pull requests")

(when (executable-find magithub-hub-executable)
  (magithub-toggle-pull-requests)
  (magithub-toggle-issues))

(provide 'magithub-issue-status)
