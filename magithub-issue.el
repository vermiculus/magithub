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
  (mapcar #'magithub-issue--process-line
          (magithub--command-output "issue")))

(defun magithub-issue--insert (issue)
  "Insert an `issue' as a Magit section into the buffer."
  (when issue
    (magit-insert-section (magithub-issue issue)
      (let ((formats magithub-issue-format))
        (while formats
          (let* ((key (car formats))
                 (fmt (cadr formats)))
            (insert (format fmt (plist-get issue key))))
          (setq formats (cddr formats))))
      (insert ?\n))))

(defun magithub-issue-browse (issue)
  "Visits `issue' in the browser."
  (interactive (list (magit-section-value
                      (magit-current-section))))
  (if (plist-member issue :url)
      (browse-url (plist-get issue :url))
    (user-error "Issue section does not have a URL")))

(defvar magit-magithub-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    map)
  "Keymap for `magithub-issue' sections.")

(defun magithub-issue--insert-section ()
  "Insert GitHub issues if appropriate."
  (when (magithub-github-repository-p)
    (magit-insert-section (magithub-issue-all)
      (magit-insert-heading "GitHub Issues and Pull Requests")
      (mapc #'magithub-issue--insert
            (magithub-issue-list)))))

;;; Hook into the status buffer
(add-hook 'magit-status-sections-hook
          #'magithub-issue--insert-section t)

(provide 'magithub-issue)
;;; magithub-issue.el ends here
