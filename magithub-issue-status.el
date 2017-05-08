(require 's)
(require 'magit)
(require 'magithub-issue)
(require 'magithub-label)

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
    (define-key map [remap magit-visit-thing] #'magithub-pull-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-pull-request-list' sections.")

(defvar magit-magithub-pull-request-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-pull-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-pull-request-list' sections.")

(defun magithub-issue--label-string (issue)
  (let-alist issue
    (mapconcat #'magithub-label-propertize .labels " ")))

(defun magithub-issue--format (issue justify-number)
  (let-alist issue
    (let ((fc fill-column)
          (indent (make-string (+ 3 justify-number) ?\ )))
      (with-temp-buffer
        (save-excursion
          (insert (format (format " %%%dd  " justify-number) .number))
          ;; 3 = 1 space before, 2 spaces after number
          ;; 2 = 2 spaces after title
          ;; justify-number = width of number
          (insert (s-word-wrap (- fc 3 justify-number 2) .title)))

        (save-excursion
          (forward-line)
          (while (not (eobp))
            (insert indent)
            (forward-line)))

        (save-excursion
          (move-to-column fc t)
          (insert (magithub-issue--label-string issue)))
        (concat (s-trim-right (buffer-string)) "\n")))))

(defun magithub-issue--insert (issue justify is-pr)
  "Insert ISSUE as a Magit section into the buffer."
  (when issue
    (let ((issue-string (magithub-issue--format issue justify)))
      (if is-pr (magit-insert-section (magithub-pull-request issue)
                  (insert issue-string))
        (magit-insert-section (magithub-issue issue)
          (insert issue-string))))))

(defun magithub-issue--format-justify ()
  (apply #'max (mapcar (lambda (i) (length (format "%d" (alist-get 'number i))))
                       (magithub--issue-list))))

(defun magithub-issue--insert-issue-section ()
  "Insert GitHub issues if appropriate."
  (when (magithub-usable-p)
    (-when-let (issues (magithub-issues))
      (let ((justify (magithub-issue--format-justify)))
        (magit-insert-section (magithub-issue-list)
          (magit-insert-heading "Issues:")
          (mapc (lambda (i) (magithub-issue--insert i justify nil))
                issues)
          (insert ?\n))))))

(defun magithub-issue--insert-pr-section ()
  "Insert GitHub pull requests if appropriate."
  (magithub-feature-maybe-idle-notify
   'pull-request-merge
   'pull-request-checkout)
  (when (magithub-usable-p)
    (-when-let (pull-requests (magithub-pull-requests))
      (let ((justify (magithub-issue--format-justify)))
        (magit-insert-section (magithub-pull-request-list)
          (magit-insert-heading "Pull Requests:")
          (mapc (lambda (i) (magithub-issue--insert i justify t))
                pull-requests)
          (insert ?\n))))))

(defun magithub-issue-browse (issue)
  "Visits ISSUE in the browser.
Interactively, this finds the issue at point."
  (interactive (list (or (magithub-issue-at-point)
                         (magithub-issue-completing-read-issues))))
  (magithub-issue--browse issue))

(defun magithub-pull-browse (pr)
  "Visits PR in the browser.
Interactively, this finds the pull request at point."
  (interactive (list (or (magithub-pull-request-at-point)
                         (magithub-issue-completing-read-pull-requests))))
  (magithub-issue--browse pr))

(defun magithub-issue--browse (issue-or-pr)
  "Visits ISSUE-OR-PR in the browser.
Interactively, this finds the issue at point."
  (-when-let (url (alist-get 'html_url issue-or-pr))
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

(magithub-toggle-pull-requests)
(magithub-toggle-issues)

(provide 'magithub-issue-status)
