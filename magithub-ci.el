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
(require 'magithub-pr)

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
             (magithub-usable-p)
             (magit-get-upstream-remote
              (magit-get-current-branch)))
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

(defun magithub-pull-request-pr->branch (pull-request)
  "Does not handle cases where the local branch has been renamed."
  (let-alist pull-request .head.ref))

(define-error 'magithub-error-ambiguous-branch "Ambiguous Branch" 'magithub-error)
(defun magithub-pull-request-branch->pr (branch)
  "This is a hueristic; it's not 100% accurate.
It may fail if branch.BRANCH.magithub.sourcePR is unset AND the
fork has multiple branches named BRANCH."
  (if-let ((source (magit-get "branch" branch "magithub" "sourcePR")))
      (magithub-pull-request (magithub-repo) (string-to-number source))
    ;; PR not recorded; search for it
    (let ((repo (magithub-repo-from-remote (magit-get-push-remote branch))))
      (when (alist-get 'fork repo)
        (let* ((guess-head (format "%s:%s" (magit-get-push-remote branch) branch))
               (prs (ghubp-get-repos-owner-repo-pulls (magithub-repo) :head guess-head)))
          (pcase (length prs)
            (0)    ; this branch does not seem to correspond to any PR
            (1 (magit-set (number-to-string (alist-get 'number (car prs)))
                          "branch" branch "magithub" "sourcePR")
               (car prs))
            (_ ;; todo: currently unhandled
             (signal 'magithub-error-ambiguous-branch
                     (list :prs prs
                           :guess-head guess-head
                           :repo-from-remote (alist-get 'full_name repo)
                           :source-repo (alist-get 'full_name (magithub-repo)))))))))))


(defun magithub-api-rate-limit ()
  "Get the `.rate' object of /rate_limit from response headers."
  (when ghub-response-headers
    (if (assoc-string "X-RateLimit-Limit" ghub-response-headers)
        (let* ((headers (list "X-RateLimit-Limit" "X-RateLimit-Remaining" "X-RateLimit-Reset"))
               (headers (mapcar (lambda (x) (string-to-number (ghubp-header x))) headers)))
          `((limit     . ,(nth 0 headers))
            (remaining . ,(nth 1 headers))
            (reset     . ,(seconds-to-time
                           (nth 2 headers)))))
      'disabled)))

(defun magithub-ci-status--get-default-ref (&optional branch)
  "The ref to use for CI status based on BRANCH.

Handles cases where the local branch's name is different than its
remote counterpart."
  (if-let ((pull-request (magithub-pull-request-branch->pr (magit-get-current-branch))))
      (let-alist pull-request .head.sha)
    (when-let ((push-branch (magit-get-push-branch (or branch (magit-get-current-branch)))))
      (cdr (magit-split-branch-name push-branch)))))

(defun magithub-ci-status (ref)
  (when (stringp ref)
    (if (magit-rebase-in-progress-p)
        ;; avoid rate-limiting ourselves
        (magithub-debug-message "skipping CI status checks while in rebase")
      (condition-case _
          (magithub-cache :ci-status
            `(ghubp-get-repos-owner-repo-commits-ref-status
              ',(magithub-repo) ,ref)
            (format "Getting CI status for %s..."
                    (if (magit-branch-p ref) (format "branch `%s'" ref)
                      (substring ref 0 6))))
        (ghub-404
         '((state . "error")
           (total_count . 0)
           (magithub-message . "ref not found on remote")))))))

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

(defun magithub-ci-refresh (even-if-offline)
  "Invalidate the CI cache and refresh the buffer.
If EVEN-IF-OFFLINE is non-nil, we'll still refresh (that is,
we'll hit the API) if Magithub is offline."
  (interactive "P")
  (let ((magithub-cache (if even-if-offline nil magithub-cache)))
    (magithub-cache-without-cache :ci-status
      (ignore (magithub-ci-status (magithub-ci-status--get-default-ref)))))
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defun magithub-insert-ci-status-header ()
  (let* ((ref (magithub-ci-status--get-default-ref))
         (checks (magithub-ci-status ref))
         (indent (make-string 10 ?\ )))
    (when checks
      (magit-insert-section (magithub-ci-status
                             `(magithub-ci-ref . ,ref)
                             'collapsed)
        (insert (format "%-10s" "Status: "))
        (insert (magithub-ci--status-header checks))
        (magit-insert-heading)
        (magit-insert-section (magithub-ci-status-meta
                               `(magithub-ci-ref . ,ref))
          (insert
           (propertize
            (concat
             indent
             (propertize (concat "Checks for ref: "
                                 (propertize ref 'face 'magit-branch-local))
                         'face 'magit-dimmed))
            'keymap magit-magithub-ci-status-section-map))
          (magit-insert-heading))
        (dolist (status (alist-get 'statuses checks))
          (magit-insert-section (magithub-ci-status
                                 `(magithub-ci-url . ,(alist-get 'target_url status)))
            (insert indent)
            (insert (magithub-ci--status-propertized status))
            (magit-insert-heading)))))))

(defun magithub-ci--status-header (checks)
  (pcase (alist-get 'total_count checks)
    (0 (format "%s  (%s)"
               (magithub-ci--status-propertized checks)
               (or (alist-get 'magithub-message checks)
                   "it seems checks have not yet begun")))
    (1 (magithub-ci--status-propertized checks))
    (_ (let* ((overall-status (alist-get 'state checks))
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
        (description   (alist-get 'description status))
        (context       (alist-get 'context status)))
    (let-alist (magithub-ci--status-spec status-string)
      (concat (propertize (or .display status-string)
                          'face .face)
              (when description
                (format " %s" description))
              (when context
                (propertize (format " %s" context)
                            'face 'magit-dimmed))))))

(magithub--deftoggle magithub-toggle-ci-status-header
  magit-status-headers-hook #'magithub-maybe-insert-ci-status-header "the CI header")

(magithub-toggle-ci-status-header)

(provide 'magithub-ci)
;;; magithub-ci.el ends here
