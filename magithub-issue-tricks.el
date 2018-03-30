;;; magithub-issue-tricks.el ---                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Sean Allred

;; Author: Sean Allred <code@seanallred.com>

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

;;; Code:

(require 'magit)
(require 'magithub-issue)

(defcustom magithub-hub-executable "hub"
  "The hub executable used by Magithub."
  :group 'magithub
  :package-version '(magithub . "0.1")
  :type 'string)

(defmacro magithub-with-hub (&rest body)
  `(let ((magit-git-executable magithub-hub-executable)
         (magit-pre-call-git-hook nil)
         (magit-git-global-arguments nil))
     ,@body))

;;;###autoload
(defun magithub-pull-request-merge (pull-request &optional args)
  "Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default."
  (interactive (list (magithub-issue-completing-read-pull-requests)
                     (magit-am-arguments)))
  (unless (executable-find magithub-hub-executable)
    (user-error "This hasn't been supported in elisp yet; please install/configure `hub'"))
  (unless (member pull-request (magithub-pull-requests))
    (user-error "Unknown pull request %S" pull-request))
  (let-alist pull-request
    (magithub-with-hub
     (magit-run-git-sequencer "am" args .html_url))
    (message "#%d has been applied" .number)))

(provide 'magithub-issue-tricks)
;;; magithub-issue-tricks.el ends here
