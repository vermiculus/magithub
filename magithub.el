;;; magithub.el --- Magit interfaces for GitHub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/vermiculus/magithub
;; Package-Requires: ((emacs "25") (magit "2.12") (s "1.12.0") (ghub+ "0.3") (git-commit "2.12") (markdown-mode "2.3"))
;; Package-Version: 0.1.7

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

;; Magithub is a Magit-based interface to GitHub.
;;
;; Integrated into Magit workflows, Magithub lets you interact with
;; your GitHub repositories and manage your work/play from emacs:
;;
;;  - push brand-new local repositories up to GitHub
;;  - create forks of existing repositories
;;  - submit pull requests upstream
;;  - view and create issues
;;  - view, create, and edit comments
;;  - view status checks (e.g., Travis CI)
;;  - manage labels and assignees
;;  - view/visit notifications
;;  - write personal notes on issues for reference later
;;  - and probably more...
;;
;; Press `H' in the status buffer to get started -- happy hacking!

;;; Code:

(require 'magit)
(require 'magit-process)
(require 'cl-lib)
(require 's)
(require 'dash)
(require 'ghub+)

(require 'magithub-core)
(require 'magithub-issue)
(require 'magithub-ci)
(require 'magithub-issue-post)
(require 'magithub-issue-tricks)
(require 'magithub-orgs)
(require 'magithub-dash)

;;;###autoload (autoload 'magithub-dispatch-popup "magithub" nil t)
(magit-define-popup magithub-dispatch-popup
  "Popup console for dispatching other Magithub popups."
  'magithub-commands
  :variables '((?C "Settings..." magithub-settings-popup))
  :actions '("Actions"
             (?d "Dashboard" magithub-dashboard)
             (?H "Browse on GitHub" magithub-browse)
             (?c "Create on GitHub" magithub-create)
             (?f "Fork this repo" magithub-fork)
             (?i "Submit an issue" magithub-issue-new)
             (?p "Submit a pull request" magithub-pull-request-new)
             "Meta"
             (?& "Request a feature or report a bug" magithub--meta-new-issue)
             (?h "Ask for help on Gitter" magithub--meta-help)))

;;;###autoload
(eval-after-load 'magit
  '(progn
     (magit-define-popup-action 'magit-dispatch-popup
       ?H "Magithub" #'magithub-dispatch-popup ?!)
     (define-key magit-status-mode-map
       "H" #'magithub-dispatch-popup)))

(defun magithub-browse ()
  "Open the repository in your browser."
  (interactive)
  (unless (magithub-github-repository-p)
    (user-error "Not a GitHub repository"))
  (magithub-repo-visit (magithub-repo)))

(defvar magithub-after-create-messages
  '("Don't be shy!"
    "Don't let your dreams be dreams!")
  "One of these messages will be displayed after you create a
GitHub repository.")

(defun magithub-create (repo &optional org)
  "Create REPO on GitHub.

If ORG is non-nil, it is an organization object under which to
create the new repository.  You must be a member of this
organization."
  (interactive (if (or (not (magit-toplevel)) (magithub-github-repository-p))
                   (list nil nil)
                 (let* ((ghub-username (ghubp-username)) ;performance
                        (account (magithub--read-user-or-org))
                        (priv (magithub-confirm-no-error 'create-repo-as-private))
                        (reponame (magithub--read-repo-name account))
                        (desc (read-string "Description (optional): ")))
                   (list
                    `((name . ,reponame)
                      (private . ,priv)
                      (description . ,desc))
                    (unless (string= ghub-username account)
                      `((login . ,account)))))))
  (when (magithub-github-repository-p)
    (error "Already in a GitHub repository"))
  (if (not (magit-toplevel))
      (when (magithub-confirm-no-error 'init-repo-after-create)
        (magit-init default-directory)
        (call-interactively #'magithub-create))
    (with-temp-message "Creating repository on GitHub..."
      (setq repo
            (magithub-request
             (if org
                 (ghubp-post-orgs-org-repos org repo)
               (ghubp-post-user-repos repo)))))
    (magithub--random-message "Creating repository on GitHub...done!")
    (magit-status-internal default-directory)
    (magit-remote-add "origin" (magithub-repo--clone-url repo))
    (magit-refresh)
    (when (magit-rev-verify "HEAD")
      (magit-push-popup))))

(defun magithub--read-user-or-org ()
  "Prompt for an account with completion.

Candidates will include the current user and all organizations,
public and private, of which they're a part.  If there is only
one candidate (i.e., no organizations), the single candidate will
be returned without prompting the user."
  (let ((user (ghubp-username))
        (orgs (ghubp-get-in-all '(login)
                (magithub-orgs-list)))
        candidates)
    (setq candidates orgs)
    (when user (push user candidates))
    (cl-case (length candidates)
      (0 (user-error "No accounts found"))
      (1 (car candidates))
      (t (completing-read "Account: " candidates nil t)))))

(defun magithub--read-repo-name (for-user)
  (let* ((prompt (format "Repository name: %s/" for-user))
         (dirnam (file-name-nondirectory (substring default-directory 0 -1)))
         (valid-regexp (rx bos (+ (any alnum "." "-" "_")) eos))
         ret)
    ;; This is not very clever, but it gets the job done.  I'd like to
    ;; either have instant feedback on what's valid or not allow users
    ;; to enter invalid names at all.  Could code from Ivy be used?
    (while (not (s-matches-p valid-regexp
			     (setq ret (read-string prompt nil nil dirnam))))
      (message "invalid name")
      (sit-for 1))
    ret))

(defun magithub--random-message (&optional prefix)
  (let ((msg (nth (random (length magithub-after-create-messages))
                  magithub-after-create-messages)))
    (if prefix (format "%s  %s" prefix msg) msg)))

(defun magithub-fork ()
  "Fork 'origin' on GitHub."
  (interactive)
  (unless (magithub-github-repository-p)
    (user-error "Not a GitHub repository"))
  (magithub-confirm 'fork)
  (let* ((repo (magithub-repo))
         (fork (with-temp-message "Forking repository on GitHub..."
                 (magithub-request
                  (ghubp-post-repos-owner-repo-forks repo)))))
    (when (magithub-confirm-no-error 'fork-create-spinoff)
      (call-interactively #'magit-branch-spinoff))
    (magithub--random-message
     (let-alist repo (format "%s/%s forked!" .owner.login .name)))
    (let-alist fork
      (when (magithub-confirm-no-error 'fork-add-me-as-remote .owner.login)
        (magit-remote-add .owner.login (magithub-repo--clone-url fork))
        (magit-set .owner.login "branch" (magit-get-current-branch) "pushRemote")))
    (let-alist repo
      (when (magithub-confirm-no-error 'fork-set-upstream-to-me .owner.login)
        (call-interactively #'magit-set-branch*merge/remote)))))

(defvar magithub-clone-history nil
  "History for `magithub-clone' prompt.")

(defun magithub-clone--get-repo ()
  "Prompt for a user and a repository.
Returns a sparse repository object."
  (let ((user (ghubp-username))
        (repo-regexp  (rx bos (group (+ (not (any " "))))
                          "/" (group (+ (not (any " ")))) eos))
        repo)
    (while (not (and repo (string-match repo-regexp repo)))
      (setq repo (read-from-minibuffer
                  (concat
                   "Clone GitHub repository "
                   (if repo "(format is \"user/repo\"; C-g to quit)" "(user/repo)")
                   ": ")
                  (when user (concat user "/"))
                  nil nil 'magithub-clone-history)))
    `((owner (login . ,(match-string 1 repo)))
      (name . ,(match-string 2 repo)))))

(defcustom magithub-clone-default-directory nil
  "Default directory to clone to when using `magithub-clone'.
When nil, the current directory at invocation is used."
  :type 'directory
  :group 'magithub)

(defun magithub-clone (repo dir)
  "Clone REPO.
Banned inside existing GitHub repositories if
`magithub-clone-default-directory' is nil.

See also `magithub-preferred-remote-method'."
  (interactive (let* ((repo (magithub-clone--get-repo))
                      (repo (or (magithub-request
                                 (ghubp-get-repos-owner-repo repo))
                                (let-alist repo
                                  (user-error "Repository %s/%s does not exist"
                                              .owner.login .name))))
                      (name (alist-get 'name repo))
                      (dirname (read-directory-name
                                "Destination: "
                                magithub-clone-default-directory
                                name nil name)))
                 (list repo dirname)))
  ;; Argument validation
  (unless (called-interactively-p 'any)
    (unless (setq repo (magithub-request
                        (ghubp-get-repos-owner-repo repo)))
      (let-alist repo
        (user-error "Repository %s/%s does not exist"
                    .owner.login .name))))
  (let ((parent (file-name-directory dir)))
    (unless (file-exists-p parent)
      (when (magithub-confirm 'clone-create-directory parent)
        (mkdir parent t))))
  (unless (file-writable-p dir)
    (user-error "%s is not writable" dir))

  (let-alist repo
    (when (magithub-confirm-no-error 'clone .full_name dir)
      (let (set-upstream set-proxy)
        (setq set-upstream
              (and .fork (magithub-confirm-no-error
			  'clone-fork-set-upstream-to-parent
                          .parent.full_name))
              set-proxy
              (and set-upstream (magithub-confirm-no-error
				 'clone-fork-set-proxy-to-upstream)))
        (condition-case _
            (let ((default-directory dir)
                  (magit-clone-set-remote.pushDefault t))
              (mkdir dir t)
              (magit-clone (magithub-repo--clone-url repo) dir)
              (while (process-live-p magit-this-process)
                (magit-process-buffer)
                (message "Waiting for clone to finish...")
                (sit-for 1))
              (when set-upstream
                (let ((upstream "upstream"))
                  (when set-proxy (magit-set upstream "magithub.proxy"))
                  (magit-remote-add upstream (magithub-repo--clone-url .parent))
                  (magit-set-branch*merge/remote (magit-get-current-branch)
						 upstream)))))))))

(defun magithub-clone--finished (user repo dir)
  "After finishing the clone, allow the user to jump to their new repo."
  (when (magithub-confirm-no-error 'clone-open-magit-status user repo dir)
    (magit-status-internal (s-chop-suffix "/" dir))))

(defun magithub-visit-thing ()
  (interactive)
  (user-error
   (with-temp-buffer
     (use-local-map magithub-map)
     (substitute-command-keys
      "Deprecated; use `\\[magithub-browse-thing]' instead"))))

(provide 'magithub)
;;; magithub.el ends here
