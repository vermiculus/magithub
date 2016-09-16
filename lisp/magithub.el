;;; magithub.el --- Magit interfaces for GitHub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: git, tools, vc
;; Package-Requires: ((magit "2.8.0") (dash "20160820.501"))

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

;; Magithub is an interface to GitHub implemented as an Emacs package.
;; It uses the command-line utility `hub' to interface with GitHub.
;; See <hub.github.com> for more details.

;; Requires hub 2.2.8

;;; Code:

(require 'magit-process)
(require 'dash)

(defcustom magithub-hub-executable "hub"
  "The hub executable used by Magithub."
  :group 'magithub
  :type 'string)

(defun magithub-command (command &optional args)
  "Run COMMAND synchronously using `magithub-hub-executable'.
This is a thin wrapper around `magit-git-command-topdir'."
  (unless (executable-find magithub-hub-executable)
    (user-error "Please install hub from hub.github.com"))
  (let ((magit-git-executable magithub-hub-executable))
    (magit-run-git-with-editor command args)))

(defun magithub-create ()
  "Push the current repository as a new GitHub repository.
If DESCRIPTION is non-nil, it is the repository description."
  (interactive)
  (magithub-command "create" (magithub-create-arguments)))

(magit-define-popup magithub-dispatch-popup
  "Popup console for dispatching other Magithub popups."
  'magithub-commands
  :man-page "hub"
  :actions '((?^ "Create this repository on GitHub" magithub-create-popup))
  :max-action-columns 2)

(magit-define-popup magithub-create-popup
  "Popup console for creating GitHub repositories."
  'magithub-commands
  :man-page "hub"
  :switches '((?p "Mark as private" "-p"))
  :actions '((?x "Create this repository on GitHub" magithub-create))
  :options '((?d "Description" "--description=")
             (?h "Homepage" "--homepage=")))

;; Integrate into the Magit dispatcher and status buffer
(magit-define-popup-action 'magit-dispatch-popup
  ?@ "Magithub" 'magithub-dispatch-popup ?!)
(define-key magit-status-mode-map (kbd "@")
  #'magithub-dispatch-popup)

(provide 'magithub)
;;; magithub.el ends here
