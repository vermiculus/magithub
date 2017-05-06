;;; magithub-ci.el --- Show CI status as a magit-status header  -*- lexical-binding: t; -*-

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

;; Provide the CI status of "origin" in the Magit status buffer.

;;; Code:

(require 'magit)
(require 'magit-section)
(require 'magit-popup)
(require 'dash)
(require 's)

(require 'magithub-core)
(require 'magithub-cache)

(defun magithub-ci-enabled-p ()
  "Non-nil if CI is enabled for this repository.
If magithub.ci.enabled is not set, CI is considered to be enabled."
  (member (magit-get "magithub" "ci" "enabled") '(nil "yes")))
(defun magithub-ci--set-enabled (val)
  (magit-set (if val "yes" "no") "magithub" "ci" "enabled"))
(defun magithub-ci-disable ()
  "Disable CI for this repository."
  (magithub-ci--set-enabled nil))
(defun magithub-ci-enable ()
  "Enable CI for this repository."
  (magithub-ci--set-enabled t))

(defun magithub-maybe-insert-ci-status-header ()
  "If this is a GitHub repository, insert the CI status header."
  (when (and (magithub-ci-enabled-p)
             (magithub-usable-p))
    (magithub-insert-ci-status-header)))

(defun magithub-ci-toggle ()
  "Toggle CI integration."
  (interactive)
  (if (magithub-ci-enabled-p)
      (magithub-ci-disable)
    (magithub-ci-enable))
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(magit-define-popup-action 'magithub-dispatch-popup
  ?~ "Toggle CI for this repository" #'magithub-ci-toggle ?`)

(defun magithub-ci-status (ref)
  (magithub-cache :ci-status
    `(ghubp-get-repos-owner-repo-commits-ref-status
      '((repo . ,(magithub-source-repo))
        (ref . ,ref)))
    (format "Getting CI status for %s..." (substring ref 0 6))))

(defun magithub-ci-status--last-commit ()
  "Find the commit considered to have the current CI status.
Right now, this finds the most recent commit without

    [ci skip]

or

    [skip ci]

in the commit message.

This does not account for the fact that the current commit might
not yet be pushed.

See the following resources:

 - https://docs.travis-ci.com/user/customizing-the-build#Skipping-a-build
 - https://circleci.com/docs/skip-a-build/"
  (let* ((args '("--invert-grep"
                 "--grep=\\[ci skip\\]"
                 "--grep=\\[skip ci\\]"
                 "--format=oneline"
                 "--max-count=1"))
         (output (magit-git-lines "log" args)))
    (car (split-string (car output)))))

(defvar magithub-ci-status-alist
  '((nil       . ((display . "None")    (face . magithub-ci-no-status)))
    ("error"   . ((display . "Error")   (face . magithub-ci-error)))
    ("failure" . ((display . "Failure") (face . magithub-ci-failure)))
    ("pending" . ((display . "Pending") (face . magithub-ci-pending)))
    ("success" . ((display . "Success") (face . magithub-ci-success)))))
(defconst magithub-ci-status--unknown
  '((face . magithub-ci-unknown)))

(defface magithub-ci-no-status
  '((((class color)) :inherit magit-dimmed))
  "Face used when CI status is `no-status'."
  :group 'magithub-faces)

(defface magithub-ci-error
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `error'."
  :group 'magithub-faces)

(defface magithub-ci-pending
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `pending'."
  :group 'magithub-faces)

(defface magithub-ci-success
  '((((class color)) :inherit magit-signature-good))
  "Face used when CI status is `success'."
  :group 'magithub-faces)

(defface magithub-ci-failure
  '((((class color)) :inherit magit-signature-bad))
  "Face used when CI status is `'"
  :group 'magithub-faces)

(defface magithub-ci-unknown
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `unknown'."
  :group 'magithub-faces)

(defun magithub-ci-visit (ref)
  "Jump to CI with `browse-url'."
  (interactive (list (magit-rev-parse (magit-commit-at-point))))
  (let (done)
    (when (null ref)
      (pcase (magit-section-value (magit-current-section))
        (`(magithub-ci-url . ,url)
         (browse-url url)
         (setq done t))
        (`(magithub-ci-ref . ,secref)
         (setq ref secref))))
    (unless done
      (let* ((urls (alist-get 'statuses (magithub-ci-status ref)))
             (status
              (cond
               ((= 1 (length urls)) (car urls))
               (urls (magithub--completing-read
                      "Status service: " urls
                      #'magithub-ci--format-status)))))
        (let-alist status
          (when (or (null .target_url) (string= "" .target_url))
            (user-error "No Status URL detected"))
          (browse-url .target_url))))))

(defun magithub-ci--format-status (status)
  (let-alist status
    (format "(%s) %s: %s"
            (let ((spec (magithub-ci--status-spec .state)))
              (alist-get 'display spec .state))
            .context
            .description)))

(defvar magit-magithub-ci-status-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-ci-visit)
    (define-key map [remap magit-refresh] #'magithub-ci-refresh)
    map)
  "Keymap for `magithub-ci-status' header section.")

(defun magithub-ci-refresh ()
  "Invalidate the CI cache and refresh the buffer."
  (interactive)
  (magithub-cache-clear)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defun magithub-insert-ci-status-header ()
  (let* ((ref (or "1fed26cce41e07b15fc44a3a828669ef79104bbe"
                  (magithub-ci-status--last-commit)))
         (checks (magithub-ci-status ref))
         (overall-status (or (cdr (assoc-string (alist-get 'status checks)
                                                magithub-ci-status-alist))
                             magithub-ci-status--unknown)))
    (when checks
      (magit-insert-section (magithub-ci-status
                             `(magithub-ci-ref . ,ref))
        (insert (format "%-10s" "CI: "))
        (insert (magithub-ci--status-header checks))
        (dolist (status (alist-get 'statuses checks))
          (insert ?\n)
          (magit-insert-section (magithub-ci-status
                                 `(magithub-ci-url . ,(alist-get 'target_url status)))
            (insert "  ")
            (insert (magithub-ci--status-propertized status))))
        (insert ?\n)))))

(defun magithub-ci--status-header (checks)
  (let ((total (alist-get 'total_count checks)))
    (if (and total (= 1 total))
        (magithub-ci--status-propertized checks)
      (let* ((overall-status (alist-get 'state checks))
             (status-spec (magithub-ci--status-spec overall-status))
             (display (or (alist-get 'display status-spec) overall-status))
             (statuses (alist-get 'statuses checks))
             (passed (-filter (lambda (s) (string= "success" (alist-get 'state s)))
                              statuses)))
        (propertize (format "%s (%d/%d)" display (length passed) (length statuses))
                    'face (alist-get 'face status-spec))))))

(defun magithub-ci--status-spec (status-string)
  (or (cdr (assoc-string status-string magithub-ci-status-alist))
      magithub-ci-status--unknown))

(defun magithub-ci--status-propertized (status)
  (let ((status-string (alist-get 'state status))
        (description   (alist-get 'description status)))
    (let-alist (magithub-ci--status-spec status-string)
      (concat (propertize (or .display status-string)
                          'face .face)
              (when description
                (format " %s" description))))))

(magithub--deftoggle magithub-toggle-ci-status-header
  magit-status-headers-hook #'magithub-maybe-insert-ci-status-header "the CI header")

(when (executable-find magithub-hub-executable)
  (magithub-toggle-ci-status-header))

(provide 'magithub-ci)
;;; magithub-ci.el ends here
