(require 'magithub-issue)
(require 'markdown-mode)                ;for gfm
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(define-derived-mode magithub-issue-post-mode nil
  "Magithub Issue Post"
  "Major mode for posting GitHub issues.")
(define-derived-mode magithub-issue-edit-mode gfm-mode
  "Magithub Issue Edit"
  "Major mode for editing GitHub issues.")

(defvar-local magithub-issue--extra-data nil)
(defvar-local magithub-issue--widgets nil
  "Alist of symbols to widgets.")
(defun magithub-issue--widget-get (key)
  (alist-get key magithub-issue--widgets))
(defun magithub-issue--widget-value (key)
  (widget-value (magithub-issue--widget-get key)))

(let ((m magithub-issue-post-mode-map))
  (define-key m "b" #'magithub-issue-w-jump-to-body)
  (define-key m (kbd "C-c RET") #'magithub-issue-wsubmit-issue)
  (define-key m (kbd "C-c C-k") #'magithub-issue-wcancel))

(let ((m magithub-issue-edit-mode-map))
  (define-key m (kbd "C-c C-c C-c") #'kill-buffer-and-window))

(defvar magithub-issue-widget-map
  (let ((m (copy-keymap widget-keymap)))
    (define-key m (kbd "C-c RET") #'magithub-issue-wsubmit-issue)
    (define-key m "b" #'magithub-issue-w-jump-to-body)
    m))

(defvar magithub-issue-edit-map
  (let ((m (copy-keymap gfm-mode-map)))
    (define-key m (kbd "C-c RET") #'magithub-issue-wsubmit-issue)
    (define-key m [remap beginning-of-buffer] #'magithub-issue-w-beginning-of-buffer-dwim)
    (define-key m [remap end-of-buffer] #'magithub-issue-w-end-of-buffer-dwim)
    (define-key m (kbd "TAB") #'magithub-issue-w-next-widget-dwim)
    m))

(defun magithub-issue-w-beginning-of-buffer-dwim ()
  (interactive)
  (let ((start-of-body (magithub-issue--w-start-of-body)))
    (goto-char
     (if (= (point) start-of-body)
         (point-min)
       start-of-body))))
(defun magithub-issue-w-end-of-buffer-dwim ()
  (interactive)
  (let ((end-of-body (magithub-issue--w-end-of-body)))
    (goto-char
     (if (= (point) end-of-body)
         (point-max)
       end-of-body))))
(defun magithub-issue-w-jump-to-body ()
  (interactive)
  (goto-char (magithub-issue--w-start-of-body)))

(defun magithub-issue--w-start-of-body ()
  (save-excursion
    (goto-char (widget-get (magithub-issue--widget-get 'body) :from))
    (forward-line)
    (point)))
(defun magithub-issue--w-end-of-body ()
  (save-excursion
    (goto-char (widget-get (magithub-issue--widget-get 'body) :to))
    (backward-char 3)
    (point)))
(defun magithub-issue-w-next-widget-dwim ()
  (interactive)
  (ignore-errors
    (let ((start (magithub-issue--w-start-of-body))
          (end   (magithub-issue--w-end-of-body))
          (keys  (kbd (substitute-command-keys
                       "\\[magithub-issue-w-next-widget-dwim]"))))
      (if (or (<= (point) start) (<= end (point)))
          (call-interactively #'widget-forward)
        (when-let ((func (with-temp-buffer
                           (magithub-issue-edit-mode)
                           (key-binding keys))))
          (call-interactively func))))))

(defvar magithub-issue-title-map
  (let ((m (copy-keymap magithub-issue-widget-map)))
    (define-key m (kbd "C-j") nil)
    m))

(defun magithub-issue-edit ()
  (interactive)
  (let ((start (magithub-issue--w-start-of-body))
        (end   (magithub-issue--w-end-of-body)))
    (with-current-buffer (clone-indirect-buffer "*mgh edit*" t)
      (remove-overlays)
      (narrow-to-region start end)
      (magithub-issue-edit-mode))))

(define-widget 'magithub-issue-title 'editable-field
  "Issue / pull-request title entry"
  :keymap magithub-issue-title-map
  :tag "Title"
  :format "%t: %v \n\n")
(define-widget 'magithub-issue-labels 'checklist
  "Tag entry"
  :greedy t
  :tag "Labels"
  :format "%t:\n%v \n\n")
(define-widget 'magithub-issue-text 'text
  "Issue / pull-request body entry"
  :keymap magithub-issue-edit-map
  :tag "Body"
  :format "%t:\n%v\n\n"
  :inline nil)

(defun magithub-issue--new-form (repo issue
                                      buffer-name
                                      header
                                      show-labels-p
                                      submit-caption
                                      submit-function
                                      cancel-caption
                                      cancel-function)
  "Opens a new widget-based form for issue/PR submission.

REPO should be a full repository object.

ISSUE should be an issue object.  The `title' and `labels'
properties are respected and prepopulate the form."
  (let-alist `((repo . ,repo) (issue . ,issue))
    (with-current-buffer (generate-new-buffer buffer-name)
      (magithub-issue-post-mode)
      (setq header-line-format
            (substitute-command-keys
             (s-join " | " (list header
                                 "submit: \\[magithub-issue-wsubmit-issue]"
                                 "cancel: \\[magithub-issue-wcancel]"))))
      (push
       (cons 'title (widget-create 'magithub-issue-title
                                   :size 76
                                   .issue.title))
       magithub-issue--widgets)

      (when show-labels-p
        (push (cons 'labels
                    (let ((w (apply #'widget-create 'magithub-issue-labels
                                    (mapcar (lambda (label) `(item ,(alist-get 'name label)))
                                            (magithub-label-list)))))
                      (widget-value-set w (magithub-get-in-all '(name) .issue.labels))
                      w))
              magithub-issue--widgets))

      (push (cons 'body (widget-create 'magithub-issue-text))
            magithub-issue--widgets)

      (widget-insert "\n")
      (widget-create 'push-button :notify submit-function submit-caption)
      (widget-insert "  ")
      (widget-create 'push-button :notify cancel-function cancel-caption)
      (widget-insert "\n")
      (use-local-map magithub-issue-widget-map)
      (widget-setup)
      (magithub-issue-w-jump-to-body)
      (current-buffer))))

(defun magithub-issue-new (repo title labels)
  (interactive
   (let-alist (setq repo (magithub-source-repo nil t))
     (list repo
           (read-string (format "Issue title (%s): " .full_name))
           (when .permissions.push
             (magithub-label-read-labels "Labels: ")))))

  (let-alist repo
    (switch-to-buffer-other-window
     (magithub-issue--new-form
      repo `((title . ,title) (labels . ,labels))
      "*magithub-issue*"
      (format "Creating an issue for %s" .full_name)
      .permissions.push
      "Create new issue"
      #'magithub-issue-wsubmit-issue
      "Cancel"
      #'magithub-issue-wcancel))))

(defun magithub-pull-request-new (repo title base head)
  (interactive
   (let-alist (setq repo (magithub-source-repo))
     (list repo
           (read-string (format "Pull request title (%s/%s): " .owner.login .name))
           (magit-read-branch "Base branch" (magit-get-upstream-branch))
           (format "%s:%s" (ghub--username) (magit-read-branch "Head")))))

  (let-alist repo
    (with-current-buffer
        (magithub-issue--new-form
         repo `((title . ,title))
         "*magithub-pull-request*"
         (format "PR %s/%s (%s->%s)" .owner.login .name head base)
         nil
         "Submit new pull request"
         #'magithub-issue-wsubmit-pull-request
         "Cancel"
         #'magithub-issue-wcancel)
      (setq magithub-issue--extra-data
            `((base . ,base) (head . ,head)))
      (switch-to-buffer-other-window (current-buffer)))))

(defun magithub-issue-wsubmit-issue (&rest _)
  (interactive)

  (let ((issue `((title  . ,(s-trim (magithub-issue--widget-value 'title)))
                 (labels . ,(magithub-issue--widget-value 'labels))
                 (body   . ,(s-trim (magithub-issue--widget-value 'body))))))
    (when (s-blank-p (alist-get 'title issue))
      (user-error "Title is required"))
    (when (yes-or-no-p "Are you sure you want to submit this issue? ")
      (magithub-issue-browse
       (ghubp-post-repos-owner-repo-issues (magithub-source-repo) issue))
      (kill-buffer-and-window))))
(defun magithub-issue-wsubmit-pull-request (&rest _)
  (interactive)
  (let ((pull-request `((title  . ,(s-trim (magithub-issue--widget-value 'title)))
                        (body   . ,(s-trim (magithub-issue--widget-value 'body)))
                        (base   . ,(alist-get 'base magithub-issue--extra-data))
                        (head   . ,(alist-get 'head magithub-issue--extra-data)))))
    (when (s-blank-p (alist-get 'title pull-request))
      (user-error "Title is required"))
    (when (yes-or-no-p "Are you sure you want to submit this pull request? ")
      (when (y-or-n-p "Allow maintainers to modify this pull request? ")
        (push (cons 'maintainer_can_modify t) pull-request))
      (magithub-issue-browse
       (ghubp-post-repos-owner-repo-pulls (magithub-source-repo) pull-request))
      (kill-buffer-and-window))))

(defun magithub-issue-wcancel (&rest _)
  (interactive)
  ;; It'd be nice to have a sort of kill ring for cancelled issues
  (when (yes-or-no-p "You will lose this buffer completely; are you sure? ")
    (kill-buffer-and-window)))

(advice-add #'ghub--request
            :around
            (lambda (oldfun &rest args)
              (if (memq 'dry-api magithub-debug-mode)
                  (message "ghub-request(%S)" args)
                (apply oldfun args)))
            '(:name debug))

(provide 'magithub-issue-post)
