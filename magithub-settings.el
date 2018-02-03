;;; magithub-settings.el --- repo-specific user settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sean Allred

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

;;

;;; Code:

(require 'magit)

(defconst magithub-settings-section "magithub"
  "This string prefixes all Magithub-related git settings.")
(defconst magithub-settings-prefix "magithub"
  "This string prefixes all Magithub-related git settings.")

(defmacro magithub-settings--simple (popup key variable docstring choices default)
  (declare (indent 3) (doc-string 4))
  (unless (stringp variable)
    (error "VARIABLE must be a string: %S" variable))
  (let* ((variable (concat magithub-settings-section "." variable))
         (Nset (concat "magithub-settings--set-" variable))
         (Nfmt (concat "magithub-settings--format-" variable)))
    (let ((Sset (intern Nset))
          (Sfmt (intern Nfmt)))
      `(progn
         (defun ,Sset () ,docstring (interactive)
                (magit-popup-set-variable ,variable ,choices ,default))
         (defun ,(intern Nfmt) () ,(format "See `%s'." Nset)
                (magit-popup-format-variable ,variable ,choices ,default))
         (magit-define-popup-variable ',popup ,key ,variable ',Sset ',Sfmt)))))

(defun magithub-settings--value-or (variable default &optional accessor)
  (declare (indent 2))
  (if (magit-get variable)
      (funcall (or accessor #'magit-get) variable)
    default))

;;;###autoload (autoload 'magithub-settings-popup "magithub-settings" nil t)
(magit-define-popup magithub-settings-popup
  "Popup console for managing Magithub settings."
  'magithub-commands)

(magithub-settings--simple magithub-settings-popup ?e "enabled"
  "Enable/disable all Magithub functionality."
  '("true" "false") "true")

(defun magithub-enabled-p ()
  (magithub-settings--value-or "magithub.enabled" t
    #'magit-get-boolean))

(magithub-settings--simple magithub-settings-popup ?c "cache"
  "Controls the cache.

- `always': The cache will not be used, but it will be updated

- `never': The cache is never used.  Data is always re-retrieved
           when online.  When offline, there is simply no data.

- `whenPresent': The cache is used if it exists.  If there is
                 nothing cached, data is retrieved when online.
                 When offline, there is simply no data."
  '("always" "never" "whenPresent") "whenPresent")

(defvar magithub-settings-cache-behavior-override 'none)
(defun magithub-settings-cache-behavior ()
  (if (not (eq magithub-settings-cache-behavior-override 'none))
      magithub-settings-cache-behavior-override
    (pcase (magithub-settings--value-or "magithub.cache" "whenPresent")
      ("always" t)
      ("never" nil)
      ("whenPresent" 'when-present)
      (_ 'when-present))))

(magithub-settings--simple magithub-settings-popup ?s "status.includeStatusHeader"
  "When true, the project status header is included in
`magit-status-headers-hook'."
  '("true" "false") "true")

(defun magithub-settings-include-status-p ()
  "Non-nil if the project status header should be included."
  (magithub-settings--value-or "magithub.status.includeStatusHeader" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?i "status.includeIssuesSection"
  "When true, project issues are included in
`magit-status-sections-hook'."
  '("true" "false") "true")

(defun magithub-settings-include-issues-p ()
  "Non-nil if the issues section should be included."
  (magithub-settings--value-or "magithub.status.includeIssuesSection" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?p "status.includePullRequestsSection"
  "When true, project pull requests are included in
`magit-status-sections-hook'."
  '("true" "false") "true")

(defun magithub-settings-include-pull-requests-p ()
  "Non-nil if the pull requests section should be included."
  (magithub-settings--value-or "magithub.status.includePullRequestsSection" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?x "contextRemote"
  "Use REMOTE as the proxy.
When set, the proxy is used whenever a GitHub repository is needed."
  (magit-list-remotes) "origin")

(defun magithub-settings-context-remote ()
  "Determine the correct remote to use for issue-tracking."
  (magithub-settings--value-or "magithub.contextRemote" "origin"))

(provide 'magithub-settings)
;;; magithub-settings.el ends here
