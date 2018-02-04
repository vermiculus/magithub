;;; magithub-completion.el --- Completion using info provided by magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `completion-at-point' functions which complete issue
;; numbers etc when they are entered in commit messages.

;; Extended information is attached to completions so that `company'
;; can access it via the standard `company-capf' backend.

;;; Code:


(require 'magithub-settings)
(require 'magithub-issue)
(require 'magithub-edit-mode)


;;;###autoload
(defun magithub-completion-complete-issues ()
  "A `completion-at-point' function which completes \"#123\" issue references.
Add this to `completion-at-point-functions' in buffers where you want this to be available."
  (when (magithub-enabled-p)
    (when (looking-back "#\\([0-9]*\\)" (- (point) 10))
      (let ((start (match-beginning 1))
            (end (match-end 0))
            (prefix (match-string 1))
            completions)
        (dolist (i (magithub--issue-list))
          (let-alist i
            (let ((n (number-to-string .number)))
              (when (string-prefix-p prefix n)
                (set-text-properties 0 (length n) (list :issue i) n)
                (push n completions)))))
        (list start end (sort completions #'string<)
              :exclusive 'no
              :company-docsig (lambda (c)
                                (let-alist (get-text-property 0 :issue c)
                                  .title))
              :company-location (lambda (c)
                                  (magithub-issue-browse (get-text-property 0 :issue c)))
              :annotation-function (lambda (c)
                                     (let-alist (get-text-property 0 :issue c)
                                       .title))
              :company-doc-buffer (lambda (c)
                                    (save-window-excursion
                                      (magithub-issue-visit (get-text-property 0 :issue c))))
              )))))


;;;###autoload
(defun magithub-completion-enable ()
  "Enable completion of info from magithub in the current buffer."
  (add-to-list (make-local-variable 'completion-at-point-functions)
               'magithub-completion-complete-issues))

;;;###autoload
(defun magithub-completion-enable-everywhere ()
  "Enable completion of info from magithub in the current buffer."
  (interactive)
  (dolist (hook '(git-commit-setup-hook magithub-edit-mode-hook))
    (add-hook hook 'magithub-completion-enable)))



(provide 'magithub-completion)
;;; magithub-completion.el ends here
