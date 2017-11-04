;;; magithub-edit-mode.el --- message-editing mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: multimedia

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

;; Edit generic GitHub (markdown) content.  To be used for comments,
;; issues, pull requests, etc.

;;; Code:

(require 'markdown-mode)
(require 'git-commit)

;;;###autoload
(define-derived-mode magithub-edit-mode gfm-mode "Magithub-Edit"
  "Major mode for editing GitHub issues and pull requests."
  (font-lock-add-keywords nil `((,(git-commit-summary-regexp) 1 'magithub-edit-title t))))

(defvar-local magithub-edit-submit-function nil)
(defvar-local magithub-edit-cancel-function nil)

(defface magithub-edit-title
  '((t :inherit markdown-header-face-1))
  "Face used for the title in issues and pull requests."
  :group 'magithub-faces)

(defvar magithub-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'magithub-edit-submit)
    (define-key m (kbd "C-c C-k") #'magithub-edit-cancel)
    m))

(defun magithub-edit-submit ()
  (interactive)
  (call-interactively magithub-edit-submit-function))
(defun magithub-edit-cancel ()
  (interactive)
  (call-interactively magithub-edit-cancel-function))

(provide 'magithub-edit-mode)
;;; magithub-edit-mode.el ends here
