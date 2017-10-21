;;; magithub-faces.el --- faces of Magithub          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: faces

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

;; Holds all faces for Magithub.

;;; Code:

(require 'faces)
(require 'magit)

(defface magithub-repo
  '((((class color)) :inherit magit-branch-remote))
  "Face used for repository names."
  :group 'magithub-faces)

(defface magithub-issue-title
  '((((class color))))
  "Face used for GitHub repository names."
  :group 'magithub-faces)

(defface magithub-issue-number
  '((((class color)) :inherit magit-dimmed))
  "Face used for GitHub repository names."
  :group 'magithub-faces)

(defface magithub-user
  '((((class color)) :inherit magit-log-author))
  "Face used for GitHub repository names."
  :group 'magithub-faces)

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
  '((((class color)) :inherit success))
  "Face used when CI status is `success'."
  :group 'magithub-faces)

(defface magithub-ci-failure
  '((((class color)) :inherit error))
  "Face used when CI status is `failure'"
  :group 'magithub-faces)

(defface magithub-ci-unknown
  '((((class color)) :inherit magit-signature-untrusted))
  "Face used when CI status is `unknown'."
  :group 'magithub-faces)

(defface magithub-label '((t :box t))
  "The inherited face used for labels.
Feel free to customize any part of this face, but be aware that
`:foreground' will be overridden by `magithub-label-propertize'."
  :group 'magithub)

(defface magithub-notification-reason
  '((((class color)) :inherit magit-dimmed))
  "Face used for notification reasons."
  :group 'magithub-faces)

(provide 'magithub-faces)
;;; magithub-faces.el ends here
