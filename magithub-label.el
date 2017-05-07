(require 'ghub+)
(require 'magithub-core)

(defun magithub-label-list ()
  "Return a list of issue labels."
  (mapcar
   (lambda (label) (alist-get 'name label))
   (ghubp-get-repos-owner-repo-labels
    (magithub-source-repo))))

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

(defun magithub-label-propertize (label)
  "Propertize LABEL according to its color.
Face inherits from `magithub-label-face'."
  (let-alist label
    (propertize .name 'face (list :foreground (concat "#" .color)
                                  :inherit 'magithub-label-face))))

(provide 'magithub-label)
