(require 'ghub+)
(require 'magithub-core)

(defvar magit-magithub-label-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing]  #'magithub-label-visit)
    (define-key m [remap magit-delete-thing] #'magithub-label-remove)
    (define-key m (kbd "a") #'magithub-label-add)
    m)
  "Keymap for label sections.")

(defun magithub-label-list ()
  "Return a list of issue and pull-request labels."
  (magithub-cache :label
    '(ghubp-get-repos-owner-repo-labels
      (magithub-repo))
    "Loading labels..."))

(defun magithub-label-read-labels (prompt &optional default)
  "Read some issue labels and return a list of strings.
Available issues are provided by `magithub-label-list'.

DEFAULT is a list of pre-selected labels.  These labels are not
prompted for again."
  (let ((remaining-labels
         (cl-set-difference (magithub-label-list) default
                            :test (lambda (a b)
                                    (= (alist-get 'name a)
                                       (alist-get 'name b))))))
    (magithub--completing-read-multiple
     prompt remaining-labels
     (lambda (l) (alist-get 'name l)))))

(defun magithub-label-browse (label)
  "Visit LABEL with `browse-url'.
In the future, this will likely be replaced with a search on
issues and pull requests with the label LABEL."
  (unless (string= ghub-base-url "https://api.github.com")
    (user-error "Label browsing not yet supported on GitHub Enterprise; pull requests welcome!"))
  (let-alist (magithub-repo)
    (browse-url (format "%s/%s/%s/labels/%s"
                        (ghubp-base-html-url)
                        .owner.login .name (alist-get 'name label)))))

(defcustom magithub-label-color-replacement-alist nil
  "Make certain label colors easier to see.
In your theme, you may find that certain colors are very
difficult to see.  Customize this list to map GitHub's label
colors to their Emacs replacements."
  :group 'magithub
  :type '(alist :key-type color :value-type color))

(defun magithub-label--get-display-color (label)
  "Gets the display color for LABEL.
Respects `magithub-label-color-replacement-alist'."
  (let ((original (concat "#" (alist-get 'color label))))
    (if-let ((color (assoc-string original magithub-label-color-replacement-alist t)))
        (cdr color)
      original)))

(defun magithub-label-propertize (label)
  "Propertize LABEL according to its color.
The face used is dynamically calculated, but it always inherits
from `magithub-label'.  Customize that to affect all labels."
  (propertize (alist-get 'name label)
              'face (list :foreground (magithub-label--get-display-color label)
                          :inherit 'magithub-label)))

(defun magithub-label-color-replace (label new-color)
  "For LABEL, define a NEW-COLOR to use in the buffer."
  (interactive
   (list (magithub-thing-at-point 'label)
         (magithub-core-color-completing-read "Replace label color: ")))
  (let ((label-color (concat "#" (alist-get 'color label))))
    (if-let ((cell (assoc-string label-color magithub-label-color-replacement-alist)))
        (setcdr cell new-color)
      (push (cons label-color new-color)
            magithub-label-color-replacement-alist)))
  (when (y-or-n-p "Save customization? ")
    (customize-save-variable 'magithub-label-color-replacement-alist
                             magithub-label-color-replacement-alist
                             "Auto-saved by `magithub-label-color-replace'"))
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defun magithub-label-visit (label)
  "Visit LABEL."
  (interactive (list (magit-section-value (magit-current-section))))
  (magithub-label-browse label))

(defun magithub-label-remove (issue label)
  "From ISSUE, remove LABEL."
  (interactive (let ((s (magit-current-section)))
                 (list (magit-section-parent-value s)
                       (magit-section-value s))))
  (if (and issue label)
      (let-alist `((label . ,label)
                   (issue . ,issue)
                   (repo . ,(magithub-issue-repo issue)))
        (if (yes-or-no-p (format "Remove label %S from this issue? " .label.name))
            (prog1 (ghubp-delete-repos-owner-repo-issues-number-labels-name
                    (magithub-issue-repo issue) issue label)
              (magithub-cache-without-cache :issues
                (magit-refresh-buffer)))
          (user-error "Aborted")))
    (user-error "No label here")))

(defun magithub-label-add (issue labels)
  "To ISSUE, add LABELS."
  (interactive (list (magit-section-parent-value (magit-current-section))
                     (magithub-label-read-labels "Add labels: ")))
  (if (not (and issue labels))
      (user-error "No issue/labels")
    (if (yes-or-no-p (format "Add {%s} to %s#%s? "
                             (s-join "," (ghubp-get-in-all '(name) labels))
                             (magithub-repo-name (magithub-issue-repo issue))
                             (alist-get 'number issue)))
        (prog1 (ghubp-post-repos-owner-repo-issues-number-labels
                (magithub-issue-repo issue) issue labels)
          (magithub-cache-without-cache :issues
            (magit-refresh)))
      (user-error "Aborted"))))

(provide 'magithub-label)
