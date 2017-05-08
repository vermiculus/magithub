(require 'ghub+)
(require 'magithub-core)

(defun magithub-label-list ()
  "Return a list of issue labels."
  (mapcar
   (lambda (label) (alist-get 'name label))
   (magithub-cache :label
     '(ghubp-get-repos-owner-repo-labels
       (magithub-source-repo))
     "Loading labels...")))

(defun magithub-issue-read-labels-list (prompt &optional default)
  "Read some issue labels and return a list of strings.
Available issues are provided by `magithub-issue-label-list'.

DEFAULT is a list of pre-selected labels.  These labels are not
prompted for again."
  (magithub--completing-read-multiple
   (format "%s... %s" prompt "Issue labels (or \"\" to quit): ")
   (cl-set-difference (magithub-issue-label-list) default)))

(defun magithub-issue-read-labels (prompt &optional default)
  "Read some issue labels and return a comma-separated string.
Available issues are provided by `magithub-issue-label-list'.

DEFAULT is a comma-separated list of issues -- those issues that
are in DEFAULT are not prompted for again."
  (->> (when default (s-split "," default t))
       (magithub-issue-read-labels-list prompt)
       (s-join ",")))

(defface magithub-label-face
  '((t :box t))
  "Face for labels")

(defun magithub-label-browse (label)
  "Visit LABEL with `browse-url'.
Only GitHub.com is currently supported."
  (unless (string= ghub-base-url "https://api.github.com")
    (user-error "Label browsing not yet supported on GitHub Enterprise; pull requests welcome!"))
  (let-alist (magithub-source-repo)
    (browse-url (format "https://www.github.com/%s/%s/labels/%s"
                        .owner.login .name (alist-get 'name label)))))

(defcustom magithub-label-color-replacement-alist
  '(("#5319e7" . "orange")
    ("#128A0C" . "green"))
  "Make certain label colors easier to see."
  :group 'magithub)

(defun magithub-label--get-display-color (label)
  "Gets the display color for LABEL.
Respects `magithub-label-color-replacement-alist'."
  (let ((original (concat "#" (alist-get 'color label))))
    (-if-let (color (assoc-string original magithub-label-color-replacement-alist t))
        (cdr color) original)))

(defun magithub-label-propertize (label)
  "Propertize LABEL according to its color.
The face used is dynamically calculated, but it always inherits
from `magithub-label-face'.  Customize that to affect all labels."
  (magithub--object-propertize 'label label
    (propertize (alist-get 'name label)
                'face (list :foreground (magithub-label--get-display-color label)
                            :inherit 'magithub-label-face))))

(provide 'magithub-label)
