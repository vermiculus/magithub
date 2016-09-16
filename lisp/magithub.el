;;; magithub.el --- Magit interfaces for GitHub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: git, tools, vc
;; Package-Requires: ((magit "2.8.0"))

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

(defcustom magithub-hub-executable "hub"
  "The hub executable used by Magithub."
  :group 'magithub
  :type 'string)

(defmacro magithub--hub-command (magit-function command args)
  `(if (executable-find magithub-hub-executable)
       (let ((magit-git-executable magithub-hub-executable))
         (funcall ,magit-function command args))
     (user-error "Please install hub from hub.github.com")))

(defun magithub--command (command &optional args)
  "Run COMMAND synchronously using `magithub-hub-executable'."
  (magithub--hub-command #'magit-run-git command args))

(defun magithub--command (command &optional args)
  "Run COMMAND asynchronously using `magithub-hub-executable'.
Ensure GIT_EDITOR is set up appropriately."
  (magithub--hub-command #'magit-run-git-with-editor command args))

(magit-define-popup magithub-dispatch-popup
  "Popup console for dispatching other Magithub popups."
  'magithub-commands
  :man-page "hub"
  :actions '((?c "Create" magithub-create-popup)
             (?f "Fork" magithub-fork-popup)
             (?p "Submit a pull request" magithub-pull-request-popup))
  :max-action-columns 2)

(magit-define-popup magithub-create-popup
  "Popup console for creating GitHub repositories."
  'magithub-commands
  :man-page "hub"
  :switches '((?p "Mark as private" "-p"))
  :actions '((?c "Create this repository" magithub-create))
  :options '((?d "Description" "--description=")
             (?h "Homepage" "--homepage=")))

(magit-define-popup magithub-fork-popup
  "Popup console for forking GitHub repositories."
  'magithub-commands
  :man-page "hub"
  :switches '((?r "Don't add my fork as a remote in this repository" "--no-remote"))
  :actions '((?f "Fork the project at origin" magithub-fork)))

(unintern "magithub-pull-request-popup")

(magit-define-popup magithub-pull-request-popup
  "Popup console for creating pull requests on GitHub repositories."
  'magithub-commands
  :man-page "hub"
  :switches '((?f "Ignore unpushed commits" "-f")
              (?o "Open in my browser" "-o"))
  :options '((?b "Base branch" "--base=" magit-read-branch)
             (?h "Head branch" "--head=" magit-read-branch))
  :actions '((?P "Submit a pull request" magithub-pull-request)))

(defun magithub-create ()
  "Create the current repository on GitHub."
  (interactive)
  (magithub--command "create" (magithub-create-arguments))
  (magit-push-popup))

(defun magithub-fork ()
  "Fork 'origin' on GitHub."
  (interactive)
  (when (and (string-equal "master" (magit-rev-name "HEAD"))
             (y-or-n-p "Looks like master is checked out.  Create a new branch? "))
    (call-interactively #'magit-branch-and-checkout))
  (magithub--command "fork" (magithub-fork-arguments)))

(defun magithub-pull-request ()
  "Open a pull request to 'origin' on GitHub."
  (interactive)
  (magithub--command-with-editor "pull-request" (magithub-pull-request-arguments)))

;; Integrate into the Magit dispatcher and status buffer
(magit-define-popup-action 'magit-dispatch-popup
  ?@ "Magithub" 'magithub-dispatch-popup ?!)
(define-key magit-status-mode-map (kbd "@")
  #'magithub-dispatch-popup)

(provide 'magithub)
;;; magithub.el ends here
