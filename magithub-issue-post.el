(require 'magithub-issue)
(require 'markdown-mode)                ;for gfm
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar-local magithub-issue--widget-title nil)
(defvar-local magithub-issue--widget-labels nil)
(defvar-local magithub-issue--widget-body nil)

(defvar magithub-issue-widget-map
  (let ((m (copy-keymap widget-keymap)))
    (define-key m (kbd "C-c RET") #'magithub-issue-wsubmit)
    m))

(defvar magithub-issue-edit-map
  (let ((m (copy-keymap gfm-mode-map)))
    (define-key m (kbd "C-c RET") #'magithub-issue-wsubmit)
    m))

(defvar magithub-issue-title-map
  (let ((m (copy-keymap magithub-issue-widget-map)))
    (define-key m (kbd "C-j") nil)
    m))

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
  :tag "Issue"
  :format "%t:\n%v \n\n"
  :inline nil)

(defun magithub-issue--new-form (repo issue)
  (when (alist-get 'body issue)
    (error "issue.body not yet supported; see https://emacs.stackexchange.com/q/32674/2264"))
  (let-alist `((repo . ,repo) (issue . ,issue))
    (with-current-buffer (generate-new-buffer "*magithub issue*")
      (setq header-line-format (concat "Creating an issue for " .repo.full_name))

      (setq magithub-issue--widget-title
            (widget-create 'magithub-issue-title :size 76 .issue.title))

      (when .repo.permissions.push
        (setq magithub-issue--widget-labels
              (apply #'widget-create 'magithub-issue-labels
                     (mapcar (lambda (label) `(item ,(alist-get 'name label)))
                             (magithub-label-list))))
        (widget-value-set magithub-issue--widget-labels (magithub-get-in-all '(name) .issue.labels)))

      (setq magithub-issue--widget-body (widget-create 'magithub-issue-text))

      (widget-insert "\n")
      (widget-create 'push-button :notify #'magithub-issue-wsubmit "Create new issue")
      (widget-insert "  ")
      (widget-create 'push-button :notify #'magithub-issue-wsave "Save draft")
      (widget-insert "  ")
      (widget-create 'push-button :notify #'magithub-issue-wcancel "Cancel")
      (widget-insert "\n")
      (use-local-map magithub-issue-widget-map)
      (widget-setup)
      (goto-char (widget-field-start magithub-issue--widget-title))
      (current-buffer))))

(defun magithub-issue-new (repo title labels)
  (interactive (list (magithub-source-repo nil t) nil nil))
  ;;(setcdr (assq 'push (cdr (assq 'permissions repo))) nil)
  (when (called-interactively-p)
    (let-alist repo
      (setq title (read-string (format "Issue title (%s): " .full_name))
            labels (when .permissions.push
                     (magithub-label-read-labels "Labels: ")))))
  (switch-to-buffer-other-window
   (magithub-issue--new-form repo `((title . ,title) (labels . ,labels)))))

(defun magithub-issue-wsubmit (&rest _)
  (interactive)
  (when (yes-or-no-p "Are you sure you want to submit this issue? ")
    (let ((issue `((title . ,(widget-value magithub-issue--widget-title))
                   (labels . ,(widget-value magithub-issue--widget-labels))
                   (body . ,(widget-value magithub-issue--widget-body)))))
      (magithub-issue-browse
       (ghubp-post-repos-owner-repo-issues (magithub-source-repo) issue)))
    (kill-buffer-and-window)))
(defun magithub-issue-wsave (&rest _)
  (error "feature not implemented"))
(defun magithub-issue-wcancel (&rest _)
  (when (y-or-n-p "Save draft? ")
    (magithub-issue-wsave))
  (kill-buffer-and-window))

(provide 'magithub-issue-widget)

;;; (defun magithub--url->repo (&rest _) '((owner (login . "vermiculus")) (name . "my-new-repository")))
