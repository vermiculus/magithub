(require 'magithub-core)
(require 'magithub-issue)
(require 'magithub-label)
(require 'magithub-edit-mode)

(declare-function magithub-issue-view "magithub-issue-view.el" (issue))

(defvar-local magithub-issue--extra-data nil)

(defun magithub-issue-post-submit ()
  (interactive)
  (let ((issue (magithub-issue-post--parse-buffer))
        (repo (magithub-repo)))
    (when (s-blank-p (alist-get 'title issue))
      (user-error "Title is required"))
    (when (magithub-repo-push-p repo)
      (when-let* ((issue-labels (magithub-label-read-labels "Labels: ")))
        (push (cons 'labels issue-labels) issue)))
    (when (yes-or-no-p "Are you sure you want to submit this issue? ")
      (let ((issue (magithub-request
                    (ghubp-post-repos-owner-repo-issues repo issue))))
        (magithub-edit-delete-draft)
        (magithub-issue-view issue)))))

(defun magithub-issue-post--parse-buffer ()
  (let ((lines (split-string (buffer-string) "\n")))
    `((title . ,(s-trim (car lines)))
      (body . ,(s-trim (mapconcat #'identity (cdr lines) "\n"))))))

(defun magithub-issue-new (repo)
  (interactive (list (magithub-repo)))
  (let* ((repo (magithub-repo repo))
         (name (magithub-repo-name repo)))
    (with-current-buffer
        (magithub-edit-new (format "*magithub-issue: %s*" name)
          :header (format "Creating an issue for %s" name)
          :submit #'magithub-issue-post-submit
          :file (expand-file-name "new-issue-draft"
                                  (magithub-repo-data-dir repo))
          :template (magithub-issue--template-text "ISSUE_TEMPLATE"))
      (font-lock-add-keywords nil `((,(rx bos (group (*? any)) eol) 1
                                     'magithub-issue-title-edit t)))
      (magithub-bug-reference-mode-on)
      (magit-display-buffer (current-buffer)))))

(defun magithub-pull-request-new-from-issue (repo issue base head &optional maintainer-can-modify)
  "Create a pull request from an existing issue.
REPO is the parent repository of ISSUE.  BASE and HEAD are as
they are in `magithub-pull-request-new'."
  (interactive (if-let ((issue-at-point (thing-at-point 'github-issue)))
                   (let-alist (magithub-pull-request-new-arguments)
                     (let ((allow-maint-mod (y-or-n-p "Allow maintainers to modify this pull request? ")))
                       (unless (magit-y-or-n-p (format "Are you sure you wish to create a PR based on %s by merging `%s' into `%s'?"
                                                       (magithub-issue-reference issue-at-point) .user+head .base)
                                               'magithub-submit-pull-request)
                         (user-error "Aborting"))
                       (list .repo issue-at-point .base .head allow-maint-mod)))
                 (user-error "No issue detected at point")))
  (let ((pull-request `((head . ,head)
                        (base . ,base)
                        (issue . ,(alist-get 'number issue)))))
    (when maintainer-can-modify
      (push (cons 'maintainer_can_modify t) pull-request))
    (magithub-request
     (ghubp-post-repos-owner-repo-pulls repo pull-request))))

(defun magithub-issue--template-text (template)
  (with-temp-buffer
    (when-let* ((template (magithub-issue--template-find template)))
      (insert-file-contents template)
      (buffer-string))))

(defun magithub-issue--template-find (filename)
  "Find an appropriate template called FILENAME and returns its absolute path.

See also URL
`https://github.com/blog/2111-issue-and-pull-request-templates'"
  (let ((default-directory (magit-toplevel))
        combinations)
    (dolist (tryname (list filename (concat filename ".md")))
      (dolist (trydir (list default-directory (expand-file-name ".github/")))
        (push (expand-file-name tryname trydir) combinations)))
    (-find #'file-readable-p combinations)))

(defun magithub-remote-branches (remote)
  "Return a list of branches on REMOTE."
  (let ((regexp (concat (regexp-quote remote) (rx "/" (group (* any))))))
    (--map (and (string-match regexp it)
                (match-string 1 it))
           (magit-list-remote-branch-names remote))))

(defun magithub-remote-branches-choose (prompt remote &optional default)
  "Using PROMPT, choose a branch on REMOTE."
  (magit-completing-read
   (format "[%s] %s"
           (magithub-repo-name (magithub-repo-from-remote remote))
           prompt)
   (magithub-remote-branches remote)
   nil t nil nil default))

(defun magithub-pull-request-new-arguments ()
  (unless (magit-get-push-remote)
    (user-error "Nothing on remote yet; aborting"))
  (let* ((this-repo   (magithub-read-repo "Fork's remote (this is you!) "))
         (this-repo-owner (let-alist this-repo .owner.login))
         (parent-repo (or (alist-get 'parent this-repo) this-repo))
         (this-remote (car (magithub-repo-remotes-for-repo this-repo)))
         (on-this-remote (string= (magit-get-push-remote) this-remote))
         (base-remote (car (magithub-repo-remotes-for-repo parent-repo)))
         (head-branch (let ((branch (magithub-remote-branches-choose
                                     "Head branch" this-remote
                                     (when on-this-remote
                                       (magit-get-current-branch)))))
                        (unless (magit-rev-verify (magit-get-push-branch branch))
                          (user-error "`%s' has not yet been pushed to your fork (%s)"
                                      branch (magithub-repo-name this-repo)))
                        branch))
         (base        (magithub-remote-branches-choose
                       "Base branch" base-remote
                       (when on-this-remote
                         (magit-get-upstream-branch head-branch))))
         (user+head   (format "%s:%s" this-repo-owner head-branch)))
    (when (magithub-request (ghubp-get-repos-owner-repo-pulls parent-repo nil :head user+head))
      (user-error "A pull request on %s already exists for head \"%s\""
                  (magithub-repo-name parent-repo)
                  user+head))
    `((repo . ,parent-repo)
      (base . ,base)
      (head . ,(if (string= this-remote base-remote)
                   head-branch
                 user+head))
      (user+head . ,user+head))))

(defun magithub-pull-request-new (repo base head)
  "Create a new pull request."
  (interactive (let-alist (magithub-pull-request-new-arguments)
                 (if (y-or-n-p
                      (format "You are about to create a pull request to merge branch `%s' into %s:%s; is this what you wanted to do?"
                              .user+head
                              (magithub-repo-name .repo)
                              .base))
                     (list .repo .base .head)
                   (user-error "Aborting"))))
  (let ((is-single-commit (string= (magit-rev-parse base)
                                   (magit-rev-parse (format "%s~1" head)))))
    (unless is-single-commit
      (apply #'magit-log (list (format "%s..%s" base head)) (magit-log-arguments)))
    (with-current-buffer
        (let ((template (magithub-issue--template-text "PULL_REQUEST_TEMPLATE")))
          (magithub-edit-new (format "*magithub-pull-request: %s into %s:%s*"
                                     head
                                     (magithub-repo-name repo)
                                     base)
            :header (let-alist repo (format "PR %s/%s (%s->%s)" .owner.login .name head base))
            :submit #'magithub-pull-request-submit
            :file (expand-file-name "new-pull-request-draft"
                                    (magithub-repo-data-dir repo))
            :template template
            :content (when is-single-commit
                       ;; when we only want to merge one commit
                       ;; insert that commit message as the initial content
                       (concat
                        (with-temp-buffer
                          (magit-git-insert "show" "-q" head "--format=%B")
                          (let ((fill-column (point-max)))
                            (fill-region (point-min) (point-max))
                            (buffer-string)))
                        template))))
      (font-lock-add-keywords nil `((,(rx bos (group (*? any)) eol) 1
                                     'magithub-issue-title-edit t)))
      (magithub-bug-reference-mode-on)
      (setq magithub-issue--extra-data
            `((base . ,base) (head . ,head)))
      (magit-display-buffer (current-buffer)))))

(defun magithub-pull-request-submit ()
  (interactive)
  (let ((pull-request `(,@(magithub-issue-post--parse-buffer)
                        (base . ,(alist-get 'base magithub-issue--extra-data))
                        (head . ,(alist-get 'head magithub-issue--extra-data)))))
    (when (s-blank-p (alist-get 'title pull-request))
      (user-error "Title is required"))
    (when (yes-or-no-p "Are you sure you want to submit this pull request? ")
      (when (y-or-n-p "Allow maintainers to modify this pull request? ")
        (push (cons 'maintainer_can_modify t) pull-request))
      (let ((pr (condition-case _
                    (magithub-request
                     (ghubp-post-repos-owner-repo-pulls (magithub-repo) pull-request))
                  (ghub-422
                   (user-error "This pull request already exists!")))))
        (magithub-edit-delete-draft)
        (magithub-issue-view pr)))))

(provide 'magithub-issue-post)
