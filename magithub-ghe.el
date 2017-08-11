;;; magithub-ghe.el --- Github Enterprise integration for Magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Sean Allred, Jorge Dias

;; Author: Sean Allred <code@seanallred.com>
;;         Jorge Dias <jorge@mrdias.com>
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

;; Integrate with github enterprise

;;; Code:

(require 'magithub-core)

(defcustom magithub-ghe-integration nil
  "Setup Github Enterprise integration."
  :type 'boolean
  :group 'magithub)

(defun magithub-auto-detect-github-api-url ()
  "Set ghub-base-url based on the domain of the repository."
  (if (magithub-github-repository-p)
      (let* ((remote-url (magit-get "remote" (magithub-source--remote) "url"))
             (domain (magithub--url->domain remote-url))
             (api-url "https://api.github.com"))
        (unless (string-equal "github.com" domain)
          (setq api-url (concat "https://" domain "/api/v3")))
        (magithub-debug-message "setting ghub-base-url: %S" api-url)
        (setq ghub-base-url api-url))))

(eval-after-load "magit"
  (if magithub-ghe-integration
      (add-hook 'magit-status-mode-hook 'magithub-auto-detect-github-api-url)))

(provide 'magithub-ghe)
;;; magithub-ghe.el ends here
