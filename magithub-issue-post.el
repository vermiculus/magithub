(require 'magithub-issue)

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

(defun magithub-issue-dir (repo)
  "Data directory for REPO."
  (expand-file-name "issues" (magithub-repo-dir repo)))

(defvar-local magithub-issue--info nil
  "Buffer-local variable for issue info")

(defun magithub-issue-save-draft (repo issue-draft)
  "Saves an issue to disk.

Right now, there is no way to automatically resume this draft,
though this feature is planned."
  (interactive magithub-issue--info)
  (let ((owner (plist-get repo-info :owner))
        (repo (plist-get repo-info :repo)))
    (save-excursion
      (goto-char 0)
      (insert (format "Owner: %s\nRepository: %s\n" owner repo)))
    (write-region (point-min) (point-max)
                  (magithub-issue-new-filename owner repo (buffer-string))
                  nil nil nil 'excl))
  (message "Issue saved"))

(defun magithub-issue-new-filename (repo)
  "Determine an appropriate absolute filename for an issue draft in REPO.
Guaranteed to not exist."
  (let-alist repo
    (let ((name ""))
      (while (file-exists-p name)
        (setq name (expand-file-name
                    (md5 (format "%d" (random)))
                    (magithub-issue-dir .owner.login .name))))
      name)))

(defun magithub-issue-new (repo issue)
  "Create a new issue.

REPO identifies the target repository for the new issue.
Interactively, this is determined by `magithub-source-repo'.

ISSUE is an issue object with at least the `title' and `labels'
properties.  Note `labels' is ignored by the API when creating
issues on repositories where you do not have push access."
  (interactive `(,(magithub-source-repo t)
                 ((title . ,(read-string "Issue title: "))
                  (labels . ,(magithub-issue-read-labels-list "Issue labels: ")))))
  (with-current-buffer (generate-new-buffer "*magithub issue*")
    (magithub-issue-mode)
    (magithub-issue--insert-headers
     `(("Repository" . ,(let-alist repo (concat .owner.login "/" .name)))
       ("Title" . ,(alist-get 'title issue))
       ("Labels" . ,(s-join "," (alist-get 'labels issue)))))
    (setq magithub-issue--info (list :repo repo :issue issue))
    (pop-to-buffer (current-buffer))))

(defconst magithub--header-field-regexp
  (rx bol (group (+ anything)) ":" (* whitespace)
      (group (*? anything))
      eol)
  "^\\(.+\\):[ \t\n]*\\(.*\\)")

(defun magithub-issue--parse-new-issue (text)
  "Parse TEXT and return an issue object.

TEXT is assumed to be of the following format:

    Title: My issue title
    Labels: bug,enhancement,...

    Start of issue body."
  (let ((parsed
         (let (ret)
           (with-temp-buffer
             (insert text)
             (mail-narrow-to-head)
             (goto-char (point-min))
             (while (not (eobp))
               (save-match-data
                 (if (not (re-search-forward magithub--header-field-regexp
                                             (line-end-position) t))
                     (forward-line)
                   (push (cons (match-string-no-properties 1)
                               (match-string-no-properties 2))
                         ret))))
             (widen)
             (forward-line)
             (push (cons 'body (buffer-substring-no-properties (point) (point-max)))
                   ret))
           ret)))
    `((title . ,(cdr (assoc-string "Title" parsed)))
      (labels . ,(mapcar #'s-trim (s-split "," (cdr (assoc-string "Labels" parsed)))))
      (body . ,(alist-get 'body parsed)))))

(defun magithub-issue--parse-current-buffer ()
  "Parse the current buffer text into an issue object."
  (magithub-issue--parse-new-issue
   (buffer-substring-no-properties
    (point-min) (point-max))))

(defun magithub-issue--insert-headers (headers)
  "Insert Header: Default-Value pairs"
  (let ((inhibit-point-motion-hooks nil) ; inhibit point from moving inside the field
        (props1 '(front-sticky t   rear-nonsticky t   field t   read-only t   intangible t face magit-header-line))
        (props2 '(front-sticky nil rear-nonsticky nil field nil read-only nil intangible nil)))
    (mapc (lambda (pair)
            (insert (apply #'propertize (format "%s: " (car pair)) props1))
            (insert (apply #'propertize (or (cdr pair) "") props2))
            (insert (apply #'propertize "\n" props1)))
          headers)
    (insert (apply #'propertize "\n\n" props1))))

(defun magithub-issue-cancel (repo issue-draft)
  "Cancel the current REPO's ISSUE-DRAFT."
  (interactive (list (plist-get magithub-issue--info :repo)
                     (magithub-issue--parse-current-buffer)))
  (when (and repo issue-draft)
    (if (y-or-n-p "Save your draft locally? ")
        (magithub-issue-save-draft repo-info)
      (when (yes-or-no-p "Your draft will be lost forever; are you sure? ")
        (kill-buffer-and-window)))))

(defun magithub-issue--confirm-submit (repo new-issue)
  "Confirm if the user wishes to submit to REPO a NEW-ISSUE."
  (let ((issue-title (let-alist new-issue .title))
        (max-len 20))
    (let-alist repo
      (yes-or-no-p
       (format "Submit [%s] to %s/%s? "
               (if (< (length issue-title) max-len) issue-title
                 (thread-first issue-title
                   (substring 0 (- max-len 3))
                   (s-trim)
                   (concat "...")))
               .owner.login .name)))))

(defun magithub-issue-submit (repo issue)
  "Submit a new issue to this repository.

REPO-INFO should be a plist with :owner and :repo string
properties.  The default is `magithub-issue--info' (set in
`magithub-issue-new')."
  (interactive (list (magithub-source-repo)
                     (magithub-issue--parse-current-buffer)))
  (unless (and repo issue)
    (magithub-error "no issue" "No issue information available."))
  (when (magithub-issue--confirm-submit repo issue)
    (let ((response (ghubp-post-repos-owner-repo-issues repo issue)))
      (unless response
        (magithub-error "issue submission failed"
                        "Failed to submit new issue."))
      (kill-buffer-and-window)
      (when (y-or-n-p (format "#%d submitted; open in your browser? "
                              (alist-get 'number response)))
        (magithub-issue-browse response)))))

(provide 'magithub-issue-post)
