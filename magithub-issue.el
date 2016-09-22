;;; magithub-issue.el --- Browse issues with Magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

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

;; Jump to issues from `magit-status'!

;;; Code:

(require 'magit)
(require 'magit-section)

(declare-function "magithub--command-output" 'magithub)
(declare-function "magithub-github-repository-p" 'magithub)

(defvar magithub-issue-format
  (list :number " %3d "
        :title " %s ")
  "These properties will be inserted in the order in which their
  found")

(defun magithub-issue--process-line (s)
  (let (number title url)
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert s)
      (goto-char 0)
      (search-forward "]")
      (setq number (string-to-number (substring s 0 (point))))
      (setq title (substring s (point)
                             (save-excursion
                               (goto-char (point-max))
                               (- (search-backward "(") 2))))
      (goto-char (point-max))
      (delete-char -2)
      (search-backward "(")
      (forward-char 2)
      (setq url (buffer-substring-no-properties (point) (point-max))))
    (list :number number
          :type (if (string-match-p (rx "/pull/" (+ digit) eos) url)
                    'pull 'issue)
          :title title
          :url url)))

(defun magithub-issue-list ()
  "Return a list of issues for the current repository."
  (magithub--cached :issues
    '(with-temp-message "Retrieving issue list..."
       (mapcar #'magithub-issue--process-line
               (magithub--command-output "issue")))))

(defun magithub-issue--insert (issue)
  "Insert an `issue' as a Magit section into the buffer."
  (when issue
    (magit-insert-section (magithub-issue issue)
      (let ((formats (or magithub-issue-format
                         (list :number " %3d " :title " %s "))))
        (while formats
          (let ((key (car formats)) (fmt (cadr formats)))
            (insert (format fmt (plist-get issue key))))
          (setq formats (cddr formats))))
      (insert ?\n))))

(defun magithub-issue-browse (issue)
  "Visits `issue' in the browser.
Interactively, this finds the issue at point.

If `issue' is nil, open the repository's issues page."
  (interactive (list (magit-section-value
                      (magit-current-section))))
  (browse-url
   (if (plist-member issue :url)
       (plist-get issue :url)
     (car (magithub--command-output "browse" '("--url-only" "--" "issues"))))))

(defvar magit-magithub-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    map)
  "Keymap for `magithub-issue' sections.")

(defvar magit-magithub-issue-list-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    map)
  "Keymap for `magithub-issue-list' sections.")

(defvar magithub--cache (make-hash-table)
  "A hash table to use as a cache.
Entries are of the form (time-entered . value).")

(defvar magithub--cache-clear-now nil
  "If non-nil, the cache will be invalidated when next accessed.")

(defvar magithub-cache-refresh-seconds 60
  "The number of seconds that have to pass for GitHub data to be
considered outdated.")

(defun magithub--cached (cache default)
  "The cached value for CACHE (set to DEFAULT if necessary)."
  (declare (indent defun))
  (when magithub--cache-clear-now
    (setq magithub--cache (make-hash-table)
          magithub--cache-clear-now nil))
  (let ((cached-value (gethash cache magithub--cache :no-value)))
    (if (or (eq cached-value :no-value)
            (< magithub-cache-refresh-seconds
               (time-to-seconds (time-since (car cached-value)))))
        (cdr (puthash cache (cons (current-time) (eval default))
                      magithub--cache))
      (cdr cached-value))))

(defun magithub-issue--insert-section ()
  "Insert GitHub issues if appropriate."
  (when (magithub-github-repository-p)
    (let* ((magithub--cache-clear-now (eq this-command #'magit-refresh))
           (issues (magithub-issue-list)))
      (magit-insert-section (magithub-issue-list)
        (magit-insert-heading "Issues and Pull Requests")
        (if issues (mapc #'magithub-issue--insert issues)
          (insert "  No issues.\n"))))))

;;; Hook into the status buffer
(add-hook 'magit-status-sections-hook #'magithub-issue--insert-section t)

(provide 'magithub-issue)
;;; magithub-issue.el ends here
