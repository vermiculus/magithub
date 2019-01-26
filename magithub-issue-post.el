;;; magithub-issue-post.el ---                   -*- lexical-binding: t; -*-

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

(require 'magithub-core)
(require 'magithub-issue)

(defun magithub-pull-request-new-from-issue
    (repo issue base head &optional maintainer-can-modify)
  "Create a pull request from an existing issue.
REPO is the parent repository of ISSUE.  BASE and HEAD are as
they are in `magithub-pull-request-new'."
  (interactive (if-let ((issue-at-point (forge-issue-at-point)))
                   (let-alist (magithub-pull-request-new-arguments)
                     (let ((allow-maint-mod (magithub-confirm-no-error
					     'pr-allow-maintainers-to-submit)))
                       (magithub-confirm 'submit-pr-from-issue
                                         (magithub-issue-reference issue-at-point)
                                         .user+head .base)
                       (list .repo issue-at-point .base .head allow-maint-mod)))
                 (user-error "No issue detected at point")))
  (let ((pull-request `((head . ,head)
                        (base . ,base)
                        (issue . ,(if (forge-issue-p issue)
                                      (oref issue number)
                                    (alist-get 'number issue))))))
    (when maintainer-can-modify
      (push (cons 'maintainer_can_modify t) pull-request))
    (magithub-request
     (ghubp-post-repos-owner-repo-pulls repo pull-request))))

(provide 'magithub-issue-post)
;;; magithub-issue-post.el ends here
