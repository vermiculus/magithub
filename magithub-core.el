;;; magithub-core.el --- core functions for magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Sean Allred

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

;; Core functions for Magithub.

;;; Code:

(require 'magit)
(require 'dash)
(require 's)
(require 'subr-x)
(require 'ghub)
(require 'ghub+)
(require 'bug-reference)
(require 'cl-lib)
(require 'markdown-mode)
(require 'parse-time)
(require 'thingatpt)
(require 'recentf)

(require 'magithub-settings)
(require 'magithub-faces)

(defconst magithub-github-token-scopes '(repo user notifications)
  "The authentication scopes Magithub requests.")

;;; Debugging utilities

(defvar magithub-debug-mode nil
  "Controls what kinds of debugging information shows.
List of symbols.

`dry-api' - don't actually make API requests
`forms' - show forms being evaluated in the cache")

(defun  magithub-debug-mode (&optional submode)
  "True if debug mode is on.
If SUBMODE is supplied, specifically check for that mode in
`magithub-debug-mode'."
  (and (listp magithub-debug-mode)
       (memq submode magithub-debug-mode)))

(defun magithub-debug-message (fmt &rest args)
  "Print a debug message.
Respects `magithub-debug-mode' and `debug-on-error'."
  (when (or magithub-debug-mode debug-on-error)
    (let ((print-quoted t))
      (message "magithub: (%s) %s"
               (format-time-string "%M:%S.%3N" (current-time))
               (apply #'format fmt args)))))

(defun magithub-debug--ghub-request-wrapper (oldfun &rest args)
  "Report ghub requests as they're being made.
Intended as around-advice for `ghub-requst'."
  (magithub-debug-message "ghub-request%S" args)
  (unless (magithub-debug-mode 'dry-api)
    (apply oldfun args)))
(advice-add #'ghub-request :around #'magithub-debug--ghub-request-wrapper)

(defcustom magithub-dir
  (expand-file-name "magithub" user-emacs-directory)
  "Data directory.
Various Magithub data (such as the cache) will be dumped into the
root of this directory.

If it does not exist, it will be created."
  :group 'magithub
  :type 'directory)
(add-to-list 'recentf-exclude
             (lambda (filename)
               (file-in-directory-p filename magithub-dir)))

;;; Turning Magithub on/off

(defmacro magithub-in-data-dir (&rest forms)
  "Execute forms in `magithub-dir'.
If `magithub-dir' does not yet exist, it and its parents will be
created automatically."
  (declare (debug t))
  `(progn
     (unless (file-directory-p magithub-dir)
       (mkdir magithub-dir t))
     (let ((default-directory magithub-dir))
       ,@forms)))

;;; Caching; Online/Offline mode

(defcustom magithub-cache-file "cache"
  "Use this file for Magithub's persistent cache."
  :group 'magithub
  :type 'file)

(defun magithub-cache-read-from-disk ()
  "Returns the cache as read from `magithub-cache-file'."
  (magithub-in-data-dir
   (when (file-readable-p magithub-cache-file)
     (with-temp-buffer
       (insert-file-contents magithub-cache-file)
       (read (current-buffer))))))

(defvar magithub-cache--cache
  (or (ignore-errors
        (magithub-cache-read-from-disk))
      (make-hash-table :test 'equal))
  "The actual cache.
Holds all information ever cached by Magithub.

Occasionally written to `magithub-cache-file' by
`magithub-cache-write-to-disk'.")

(defvar magithub-cache--needs-write nil
  "Signals that the cache has been updated.
When non-nil, the cache will be written to disk next time the
idle timer runs.")

(defvar magithub-cache--refreshed-forms nil
  "Forms that have been refreshed this session.
See also `magithub--refresh'.")

(cl-defun magithub-cache (class form &key message after-update)
  "The cached value for FORM if available.

If FORM has not been cached or its CLASS dictates the cache has
expired, FORM will be re-evaluated.

CLASS: The kind of data this is; see `magithub-cache--refresh'.

MESSAGE may be specified for intensive functions.  We'll display
this with `with-temp-message' while the form is evaluating.

AFTER-UPDATE is a function to run after the cache is updated."
  (declare (indent defun))
  (let* ((no-value-sym (cl-gensym))
         (entry (list (ghubp-get-context) class form))
         (online (magithub-online-p))
         (cached-value (gethash entry magithub-cache--cache no-value-sym))
         (value-does-not-exist (eq cached-value no-value-sym))
         (cached-value (if value-does-not-exist nil cached-value))
         make-request new-value)

    (when online
      (if (or (eq magithub-cache--refresh t)
              (eq magithub-cache--refresh class))
          ;; if we're refreshing (and we haven't seen this form
          ;; before), go ahead and make the request if it's the class
          ;; we're refreshing (or t, which encompasses all classes)
          (setq make-request (not (member entry magithub-cache--refreshed-forms)))
        (setq make-request value-does-not-exist)))

    (or (and make-request
             (prog1 (setq new-value (with-temp-message message (eval form)))
               (puthash entry new-value magithub-cache--cache)
               (unless magithub-cache--needs-write
                 (setq magithub-cache--needs-write t)
                 (run-with-idle-timer 600 nil #'magithub-cache-write-to-disk))
               (when magithub-cache--refresh
                 (push entry magithub-cache--refreshed-forms))
               (if (functionp after-update)
                   (funcall after-update new-value)
		 new-value)))
        cached-value)))

(defun magithub-maybe-report-offline-mode ()
  "Conditionally inserts the OFFLINE header.
If this is a Magithub-enabled repository and we're offline, we
insert a header notifying the user that all data shown is cached.
To aid in determining if the cache should be refreshed, we report
the age of the oldest cached information."
  (when (and (magithub-usable-p)
             (not (magithub-online-p)))
    (magit-insert-section (magithub nil t)
      (insert
       (format
	"Magithub: %s; use %s to refresh GitHub content or %s to go back online%s\n"
        (propertize "OFFLINE" 'face 'magit-head)
        (propertize
         (substitute-command-keys "\\[universal-argument] \\[magit-refresh]")
         'face 'magit-header-line-key)
        (propertize
         (substitute-command-keys "\\[magithub-dispatch-popup] C o")
         'face 'magit-header-line-key)
        (propertize "..." 'face 'magit-dimmed)))
      (magit-insert-heading)
      (let* ((msg "When Magithub is offline, no API requests are ever made \
automatically.  Even when online, cached API responses never expire, so \
they must be updated manually with %s.")
             (msg (s-word-wrap (- fill-column 10) msg))
             (msg (format msg (propertize
                               (substitute-command-keys
				"\\[universal-argument] \\[magit-refresh]")
                               'face 'magit-header-line-key))))
        (insert (format "%s\n" (replace-regexp-in-string
				(rx bol) (make-string 10 ?\ ) msg)))))))

(eval-after-load 'magit
  '(add-hook 'magit-status-headers-hook
             #'magithub-maybe-report-offline-mode
             'append))

(defun magithub-cache--time-out (time)
  "Convert TIME into a human-readable string.
Returns \"Xd Xh Xm Xs\" (counting from zero)"
  (let ((seconds (time-to-seconds time)))
    (format-time-string
     (cond
      ((< seconds 60)              "%-Ss")
      ((< seconds 3600)       "%-Mm %-Ss")
      ((< seconds 86400) "%-Hh %-Mm %-Ss")
      (t            "%-jd %-Hh %-Mm %-Ss"))
     time)))

(defun magithub-cache-write-to-disk ()
  "Write the cache to disk.
The cache is written to `magithub-cache-file' in
`magithub-dir'"
  (if (active-minibuffer-window)
      (run-with-idle-timer 600 nil #'magithub-cache-write-to-disk) ;defer
    (when magithub-cache--needs-write
      (magithub-in-data-dir
       (with-temp-buffer
         (insert (prin1-to-string magithub-cache--cache))
         (write-file magithub-cache-file)))
      (setq magithub-cache--needs-write nil)
      (magithub-debug-message "wrote cache to disk: %S"
			      (expand-file-name magithub-cache-file
						magithub-dir)))))

(defmacro magithub-cache-without-cache (class &rest body)
  "For CLASS, execute BODY without using CLASS's caches.
Use t to ignore previously cached values completely.
See also `magithub-cache--refresh'."
  (declare (indent 1) (debug t))
  `(let ((magithub-cache--refresh ,class))
     ,@body))

(add-hook 'kill-emacs-hook
          #'magithub-cache-write-to-disk)

;;; API availability checking

(define-error 'magithub-error "Magithub Error")
(define-error 'magithub-api-timeout "Magithub API Timeout" 'magithub-error)

(defvar magithub--api-last-checked
  ;; see https://travis-ci.org/vermiculus/magithub/jobs/259006323
  ;; (eval-when-compile (date-to-time "1/1/1970"))
  '(14445 17280)
  "The last time the API was available.
Used to avoid pinging GitHub multiple times a second.")

(defcustom magithub-api-timeout 3
  "Number of seconds we'll wait for the API to respond."
  :group 'magithub
  :type 'integer)

(defcustom magithub-api-low-threshold 15
  "Low threshold for API requests.
This variable is not currently respected; see tarsius/ghub#16.

If the number of available API requests drops to or below this
threshold, you'll be asked if you'd like to go offline."
  :group 'magithub
  :type 'integer)

(defcustom magithub-api-available-check-frequency 10
  "Minimum number of seconds between each API availability check.
While online (see `magithub-go-online'), we check to ensure the
API is available before making a real request. This involves a
`/rate_limit' call (or for some Enterprise instances, a `/meta'
call). Use this setting to configure how often this is done. It
will be done no more frequently than other API actions.

These calls are guaranteed to not count against your rate limit."
  :group 'magithub
  :type 'integer)

(defvar magithub--quick-abort-api-check nil
  "When non-nil, we'll assume the API is unavailable.
Do not modify this variable in code outside Magithub.")

(defvar magithub--api-offline-reason nil
  "The reason we're going offline.
Could be one of several strings:

 * authentication issue

 * response timeout

 * generic error

and possibly others as error handlers are added to
`magithub--api-available-p'.")

(defun magithub--api-available-p ()
  "Non-nil if the API is available.
Pings the API a maximum of once every ten seconds."
  (setq magithub--api-offline-reason nil)
  (when (magithub-enabled-p)
    (magithub-debug-message "checking if the API is available")
    (prog1 (when
               (progn
                 (magithub-debug-message "making sure authinfo is unlocked")
                 (ghubp-token 'magithub))
             (if (and magithub--api-last-checked
                      (< (time-to-seconds (time-since magithub--api-last-checked))
			 magithub-api-available-check-frequency))
                 (prog1 magithub--api-last-checked
                   (magithub-debug-message "used cached value for api-last-checked"))

               (magithub-debug-message "cache expired; retrieving new value for api-last-checked")
               (setq magithub--api-last-checked (current-time))

               (let (api-status error-data response)
                 (condition-case err
                     (progn
                       (with-timeout (magithub-api-timeout
                                      (signal 'magithub-api-timeout nil))
                         (setq response
                               ;; /rate_limit is free for GitHub.com.
                               ;; If rate limiting is disabled
                               ;; (i.e. GHE), try using /meta which
                               ;; should (hopefully) always work.  See
                               ;; also issue #107.
                               (or (ghubp-ratelimit)
                                   (ghub-get "/meta" nil :auth 'magithub))

                               api-status (and response t)))

                       (magithub-debug-message
			"new value retrieved for api-last-available: %S" response))

                   ;; Sometimes, the API can take a long time to respond
                   ;; (whether that's GitHub not responding or requests being
                   ;; blocked by some client-side firewall).  Handle this
                   ;; possibility gracefully.
                   (magithub-api-timeout
                    (setq error-data err
                          magithub--api-offline-reason
                          (concat "API is not responding quickly; "
                                  "consider customizing `magithub-api-timeout' "
				  "if this happens often")))

                   ;; Never hurts to be cautious :-)
                   (error
                    (setq error-data err)
                    (setq magithub--api-offline-reason
			  (format "unknown issue: %S" err))))

                 (when error-data
                   (magithub-debug-message
		    "consider reporting unknown error while checking api-available: %S"
                    error-data))

                 api-status)))
      (when magithub--api-offline-reason
        (magit-set "false" "magithub.online")
        (run-with-idle-timer 2 nil #'magithub--api-offline-reason)))))

(defun magithub--api-offline-reason ()
  "Report the reason we're going offline and go offline.
Refresh the status buffer if necessary.

See `magithub--api-offline-reason'."
  (when magithub--api-offline-reason
    (message "Magithub is now offline: %s"
             magithub--api-offline-reason)
    (setq magithub--api-offline-reason nil)))

(defalias 'magithub-api-rate-limit #'ghubp-ratelimit)

;;; Repository parsing

(defcustom magithub-github-hosts
  (list "github.com")
  "A list of top-level domains that should be recognized as GitHub hosts.
See also `magithub-github-repository-p'."
  :group 'magithub
  :type '(list string))

(defun magithub-github-repository-p ()
  "Non-nil if \"origin\" points to GitHub or a whitelisted domain.
See also `magithub-github-hosts'."
  (when-let ((origin (magit-get "remote" (magithub-settings-context-remote) "url")))
    (-some? (lambda (domain) (s-contains? domain origin))
            magithub-github-hosts)))

(defalias 'magithub--parse-url 'magithub--repo-parse-url)
(make-obsolete 'magithub--parse-url 'magithub--repo-parse-url "0.1.4")
(defun magithub--repo-parse-url (url)
  "Parse URL into its components.
URL may be of several different formats:

- git@github.com:vermiculus/magithub.git
- https://github.com/vermiculus/magithub"
  (and url
       (or (and (string-match
                 ;; git@github.com:vermiculus/magithub.git
                 (rx bol
                     (group (+? any)) ;sshuser -- git
                     "@"
                     (group (+? any)) ;domain  -- github.com
                     ":"
                     (group (+? (| alnum "-" "." "_"))) ;owner.login -- vermiculus
                     "/"
                     (group (+? (| alnum "-" "." "_"))) ;name -- magithub
                     (? ".git")
                     eol)
                 url)
                `((kind . 'ssh)
                  (ssh-user . ,(match-string 1 url))
                  (domain . ,(match-string 2 url))
                  (sparse-repo (owner (login . ,(match-string 3 url)))
                               (name . ,(match-string 4 url)))))
           (and (string-match
                 ;; https://github.com/vermiculus/magithub.git
                 ;; git://github.com/vermiculus/magithub.git
                 ;; ssh://git@github.com/vermiculus/magithub
                 ;; git+ssh://github.com/vermiculus/magithub.git
                 (rx bol
                     (or (seq "http" (? "s"))
                         (seq "ssh")
                         (seq "git" (? "+ssh")))
                     "://"
                     (group (+? any)) ;domain -- github.com
                     "/"
                     (group (+? (| alnum "-" "." "_"))) ;owner.login -- vermiculus
                     "/"
                     (group (+? (| alnum "-" "." "_"))) ;name -- magithub
                     (? ".git")
                     eol)
                 url)
                `((kind . 'http)
                  (domain . ,(match-string 1 url))
                  (sparse-repo (owner (login . ,(match-string 2 url)))
                               (name . ,(match-string 3 url))))))))

(defun magithub--url->repo (url)
  "Tries to parse a remote url into a GitHub repository object"
  (cdr (assq 'sparse-repo (magithub--repo-parse-url url))))

(defun magithub-source--sparse-repo ()
  "Returns the sparse repository object for the current context.

Only information that can be determined without API calls will be
included in the returned object."
  (magithub-repo-from-remote--sparse
   (magithub-settings-context-remote)))

(defun magithub-repo-from-remote (remote)
  (when-let ((repo (magithub-repo-from-remote--sparse remote)))
    (magithub-repo repo)))

(defun magithub-repo-from-remote--sparse (remote)
  (magithub--url->repo (magit-get "remote" remote "url")))

(defalias 'magithub-source-repo 'magithub-repo)
(make-obsolete 'magithub-source-repo 'magithub-repo "0.1.4")
(defun magithub-repo (&optional sparse-repo)
  "Turn SPARSE-REPO into a full repository object.
If SPARSE-REPO is null, the current context is used.

SPARSE-REPO may either be a partial repository object (with at
least the `.owner.login' and `.name' keys) or a string identifier
of the form `owner/name' (as in `vermiculus/magithub')."
  (if (and (stringp sparse-repo)
           (string-match (rx bos
                             (group (+? (| alnum "-" "." "_"))) ;owner.login -- vermiculus
                             "/"
                             (group (+? (| alnum "-" "." "_"))) ;name -- magithub
                             eos)
                         sparse-repo))
      (magithub-repo `((owner (login . ,(match-string 1 sparse-repo)))
                       (name . ,(match-string 2 sparse-repo))))
    (when-let ((sparse-repo (or sparse-repo (magithub-source--sparse-repo))))
      (or (magithub-cache :repo-demographics
            `(or (magithub-request
                  (ghubp-get-repos-owner-repo ',sparse-repo))
                 (and (not (magithub--api-available-p))
                      sparse-repo)))
          (when (magithub-online-p)
            (let ((magithub-cache--refresh t))
              (magithub-repo sparse-repo)))
          sparse-repo))))

;;; Repository utilities

(defvar magit-magithub-repo-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] #'magithub-repo-visit)
    m))

(defun magithub-repo-visit (repo)
  "Visit REPO on GitHub."
  (interactive (list (thing-at-point 'github-repository)))
  (if-let ((url (alist-get 'html_url repo)))
      (browse-url url)
    (user-error "No URL for repo")))

(defun magithub-repo-visit-issues (repo)
  "Visit REPO's issues on GitHub."
  (interactive (list (thing-at-point 'github-repository)))
  (if-let ((url (alist-get 'html_url repo)))
      (browse-url (format "%s/issues" url))
    (user-error "No URL for repo")))

(defun magithub-repo-name (repo)
  "Return the full name of REPO.
If the `full_name' object is present, use that.  Otherwise,
concatenate `.owner.login' and `.name' with `/'."
  (let-alist repo (or .full_name (concat .owner.login "/" .name))))

(defun magithub-repo-admin-p (&optional repo)
  "Non-nil if the currently-authenticated user can manage REPO.
REPO defaults to the current repository."
  (let-alist (magithub-repo (or repo (thing-at-point 'github-repository)))
    .permissions.admin))

(defun magithub-repo-push-p (&optional repo)
  "Non-nil if the currently-authenticated user can manage REPO.
REPO defaults to the current repository."
  (let-alist (magithub-repo (or repo (thing-at-point 'github-repository)))
    .permissions.push))

(defun magithub--repo-simplify (repo)
  "Convert full repository object REPO to a sparse repository object."
  (let (login name)
    ;; There are syntax problems with things like `,.owner.login'
    (let-alist repo
      (setq login .owner.login
            name .name))
    `((owner (login . ,login))
      (name . ,name))))

(defun magithub-repo-remotes ()
  "Return GitHub repositories in this repository.
`magit-list-remotes' is filtered to those remotes that point to
GitHub repositories."
  (delq nil (mapcar (lambda (r)
                      (when-let ((repo (magithub-repo-from-remote r)))
                        (cons r repo)))
                    (magit-list-remotes))))

(defun magithub-read-repo (prompt)
  "Using PROMPT, read a GitHub repository.
See also `magithub-repo-remotes'."
  (let* ((remotes (magithub-repo-remotes))
         (maxlen (->> remotes
                      (mapcar #'car)
                      (mapcar #'length)
                      (apply #'max)))
         (fmt (format "%%-%ds (%%s/%%s)" maxlen)))
    (magithub-repo
     (cdr (magithub--completing-read
           prompt (magithub-repo-remotes)
           (lambda (remote-repo-pair)
             (let-alist (cdr remote-repo-pair)
               (format fmt (car remote-repo-pair) .owner.login .name))))))))

(defun magithub-repo-remotes-for-repo (repo)
  (-filter (lambda (remote)
             (let-alist (list (cons 'repo repo)
                              (cons 'remote (magithub-repo-from-remote remote)))
               (and (string= .repo.owner.login
                             .remote.owner.login)
                    (string= .repo.name .remote.name))))
           (magit-list-remotes)))

;;; Feature checking

(declare-function magithub-pull-request-merge "magithub-issue-tricks"
                  (pull-request &optional args))
(declare-function magithub-maybe-insert-ci-status-header "magithub-ci" ())
(declare-function magithub-issue--insert-pr-section "magithub-issue" ())
(declare-function magithub-issue--insert-issue-section "magithub-issue" ())
(declare-function magithub-completion-enable "magithub-completion" ())
(defconst magithub-feature-list
  ;; features must only return nil if they fail to install
  `((pull-request-merge . ,(lambda ()
                             (magit-define-popup-action 'magit-am-popup
                               ?P "Apply patches from pull request"
                               #'magithub-pull-request-merge)
                             t))

    (commit-browse . ,(lambda ()
                        (define-key magit-commit-section-map "w"
                          #'magithub-commit-browse)
                        t))

    (status-checks-header . ,(lambda ()
                               (add-hook 'magit-status-headers-hook
                                         #'magithub-maybe-insert-ci-status-header
                                         t)
                               t))

    (completion . ,(lambda ()
                     (dolist (hook '(git-commit-setup-hook magithub-edit-mode-hook))
                       (add-hook hook #'magithub-completion-enable))
                     t))

    ;; order is important in this list; pull request section should
    ;; come before issues section by default
    (pull-requests-section . ,(lambda ()
                                (add-hook 'magit-status-sections-hook
                                          #'magithub-issue--insert-pr-section
                                          t)
                                t))

    (issues-section . ,(lambda ()
                         (add-hook 'magit-status-sections-hook
                                   #'magithub-issue--insert-issue-section
                                   t)
                         t)))
  "All Magit-integration features of Magithub.
See `magithub-feature-autoinject'.

- `pull-request-merge'
  Apply patches from pull requests.
  (`magithub-pull-request-merge' inserted into `magit-am-popup')

- `commit-browse'
  Browse commits using \\<magithub-map>\\[magithub-browse-thing].

- `completion'
  Enable `completion-at-point' support for #issue and @user references
  where possible.

- `issues-section'
  View issues in the `magit-status' buffer.

- `pull-requests-section'
  View pull requests in the `magit-status' buffer.

- `status-checks-header'
  View project status in the `magit-status' buffer (e.g., CI).")

(defvar magithub-features nil
  "An alist of feature-symbols to Booleans.
When a feature symbol maps to non-nil, that feature is considered
'loaded'.  Thus, to disable all messages, prepend '(t . t) to
this list.

Example:

    ((pull-request-merge . t) (other-feature . nil))

signals that `pull-request-merge' is a loaded feature and
`other-feature' has not been loaded and will not be loaded.

See `magithub-feature-list'.")

;;;###autoload
(defun magithub-feature-autoinject (feature)
  "Configure FEATURE to recommended settings.
If FEATURE is `all' or t, all known features will be loaded.  If
FEATURE is a list, then it is a list of FEATURE symbols to load.

See `magithub-feature-list' for a list of available features and
`magithub-features' for a list of currently-installed features."
  (cond
   ((memq feature '(t all))
    (mapc #'magithub-feature-autoinject
          (mapcar #'car magithub-feature-list)))
   ((listp feature)
    (mapc #'magithub-feature-autoinject feature))
   (t
    (if-let ((install (cdr (assq feature magithub-feature-list))))
        (if (functionp install)
            (if-let ((result (funcall install)))
                (add-to-list 'magithub-features (cons feature t))
              (error "feature %S failed to install: %S" feature result))
          (error "install form for %S not a function: %S" feature install))
      (user-error "unknown feature %S" feature)))))

(defun magithub-feature-check (feature)
  "Check if a Magithub FEATURE has been configured.
See `magithub-features'."
  (if (listp magithub-features)
      (let* ((p (assq feature magithub-features)))
        (if (consp p) (cdr p)
          (cdr (assq t magithub-features))))
    magithub-features))

(defun magithub-feature-maybe-idle-notify (&rest feature-list)
  "Notify user if any of FEATURES are not yet configured."
  (unless (-all? #'magithub-feature-check feature-list)
    (let ((m "Magithub features not configured: %S")
          (s "see variable `magithub-features' to turn off this message"))
      (run-with-idle-timer
       1 nil (lambda ()
               (message (concat m "; " s) feature-list)
               (add-to-list 'feature-list '(t . t) t))))))

;;; Getting help

(defun magithub--meta-new-issue ()
  "Open a new Magithub issue.
See /.github/ISSUE_TEMPLATE.md in this repository."
  (interactive)
  (browse-url "https://github.com/vermiculus/magithub/issues/new"))

(defun magithub--meta-help ()
  "Open Magithub help."
  (interactive)
  (browse-url "https://gitter.im/vermiculus/magithub"))

(defun magithub-error (err-message &optional tag trace)
  "Report a Magithub error.

ERR-MESSAGE is a string to be shown to the user.

TAG, if provided, is a user-friendly description of the error.
It defaults to ERR-MESSAGE.

If TRACE is provided, it should be an appropriate backtrace to
describe the error.  If not provided, it is retrieved."
  (unless (stringp err-message)
    ;; just in case.  it'd be embarassing if the bug-reporter was
    ;; perceived as buggy
    (setq err-message (prin1-to-string err-message)))
  (setq trace (or trace (with-output-to-string (backtrace)))
        tag (or tag err-message))
  (when (magithub-confirm-no-error 'report-error tag)
    (with-current-buffer-window
     (get-buffer-create "*magithub issue*")
     #'display-buffer-pop-up-window nil
     (when (fboundp 'markdown-mode) (markdown-mode))
     (insert
      (kill-new
       (format
        "## Automated error report

%s

### Description

%s

### Backtrace

```
%s```
"
        err-message
        (read-string "Briefly describe what you were doing: ")
        trace))))
    (magithub--meta-new-issue))
  (error err-message))

;;; Miscellaneous utilities

(defcustom magithub-datetime-format "%c"
  "The display format string for date-time values.
See also `format-time-string'."
  :group 'magithub
  :type 'string)

(defun magithub--parse-time-string (iso8601)
  "Parse ISO8601 into a time value.
ISO8601 is expected to not have a TZ component."
  (parse-iso8601-time-string (concat iso8601 "+00:00")))

(defun magithub--format-time (time)
  "Format TIME according to `magithub-datetime-format'.
TIME may be a time value or a string.

Eventually, TIME will always be a time value."
  ;; todo: ghub+ needs to convert time values for defined response fields
  (format-time-string
   magithub-datetime-format
   (or (and (stringp time)
            (magithub--parse-time-string time))
       time)))

(defun magithub--completing-read
    (prompt collection &optional format-function predicate require-match default)
  "Using PROMPT, get a list of elements in COLLECTION.
This function continues until all candidates have been entered or
until the user enters a value of \"\".  Duplicate entries are not
allowed."
  (let* ((format-function (or format-function (lambda (o) (format "%S" o))))
         (collection (if (functionp predicate) (-filter predicate collection) collection))
         (collection (magithub--zip collection format-function nil)))
    (cdr (assoc-string
          (completing-read prompt collection nil require-match
                           (when default (funcall format-function default)))
          collection))))

(defun magithub--completing-read-multiple
    (prompt collection &optional format-function predicate require-match default)
  "Using PROMPT, get a list of elements in COLLECTION.
This function continues until all candidates have been entered or
until the user enters a value of \"\".  Duplicate entries are not
allowed."
  (let ((this t) (coll (copy-tree collection)) ret)
    (while (and collection this)
      (setq this (magithub--completing-read
                  prompt coll format-function
                  predicate require-match default))
      (when this
        (push this ret)
        (setq coll (delete this coll))))
    ret))

(defconst magithub-hash-regexp
  (rx bow (= 40 (| digit (any (?A . ?F) (?a . ?f)))) eow)
  "Regexp for matching commit hashes.")

(defun magithub-usable-p ()
  "Non-nil if Magithub should do its thing."
  (and (magithub-enabled-p)
       (magithub-github-repository-p)
       (magithub-source--sparse-repo)))

(defun magithub--zip-case (p e)
  "Get an appropriate value for element E given property/function P."
  (cond
   ((null p) e)
   ((functionp p) (funcall p e))
   ((symbolp p) (plist-get e p))
   (t nil)))

(defun magithub--zip (object-list prop1 prop2)
  "Process OBJECT-LIST into an alist defined by PROP1 and PROP2.

If a prop is a symbol, that property will be used.

If a prop is a function, it will be called with the
current element of OBJECT-LIST.

If a prop is nil, the entire element is used."
  (delq nil
        (-zip-with
         (lambda (e1 e2)
           (let ((p1 (magithub--zip-case prop1 e1))
                 (p2 (magithub--zip-case prop2 e2)))
             (unless (or (and prop1 (not p1))
                         (and prop2 (not p2)))
               (cons (if prop1 p1 e1)
                     (if prop2 p2 e2)))))
         object-list object-list)))

(defun magithub--satisfies-p (preds obj)
  "Non-nil when all functions in PREDS are non-nil for OBJ."
  (while (and (listp preds)
              (functionp (car preds))
              (funcall (car preds) obj))
    (setq preds (cdr preds)))
  (null preds))

(defun magithub-section-type (section)
  "If SECTION is a magithub-type section, return the type.
For example, if

  (eq (magit-section-type SECTION) \\='magithub-issue)

return the interned symbol `issue'."
  (let* ((type (oref section type))
         (name (symbol-name type)))
    (and (string-prefix-p "magithub-" name)
         (intern (substring name 9)))))

(defvar magithub--section-value-at-point-specializations
  '((user assignee))
  "Alist of general types to specific types.
Specific types offer more relevant functionality to a given
section, but are inconvenient for
`magithub--section-value-at-point'.  This alist defines
equivalencies such that a search for the general type will also
return sections of a specialized type.")

(define-obsolete-function-alias
  'magithub-thing-at-point
  #'magithub--section-value-at-point
  "0.1.5")

;;;###autoload
(defun magithub--section-value-at-point (type)
  "Determine the thing of TYPE at point.
This is intended for use as a resolving function for
`thing-at-point'.

The following symbols are defined, but other values may work with
this function: `github-user', `github-issue', `github-label',
`github-comment', `github-repository', `github-pull-request',
`github-notification',"
  (let ((search-sym (intern (concat "magithub-" (symbol-name type))))
        this-section)
    (if (and (boundp search-sym) (symbol-value search-sym))
        (symbol-value search-sym)
      (setq this-section (magit-current-section))
      (while (and this-section
                  (not (let ((this-type (magithub-section-type this-section)))
                         (or
                          ;; exact match
                          (eq type this-type)
                          ;; equivalency
                          (thread-last magithub--section-value-at-point-specializations
                            (alist-get type)
                            (memq this-type))))))
        (setq this-section (oref this-section parent)))
      (and this-section (oref this-section value)))))

(defvar-local magithub-issue nil
  "Issue object.")

(defvar-local magithub-comment nil
  "Comment object.")

(defvar-local magithub-repo nil
  "Repository object.")

;;;###autoload
(put 'github-user 'thing-at-point
     (lambda ()
       (magithub--section-value-at-point 'user)))

;;;###autoload
(put 'github-issue 'thing-at-point
     (lambda ()
       (or magithub-issue
           (magithub--section-value-at-point 'issue))))

;;;###autoload
(put 'github-label 'thing-at-point
     (lambda ()
       (magithub--section-value-at-point 'label)))

;;;###autoload
(put 'github-comment 'thing-at-point
     (lambda ()
       (or magithub-comment
           (magithub--section-value-at-point 'comment))))

;;;###autoload
(put 'github-notification 'thing-at-point
     (lambda ()
       (magithub--section-value-at-point 'notification)))

;;;###autoload
(put 'github-repository   'thing-at-point
     (lambda ()
       (or (magithub--section-value-at-point 'repository)
           magithub-repo
           (magithub-repo))))

;;;###autoload
(put 'github-pull-request 'thing-at-point
     (lambda ()
       (or (magithub--section-value-at-point 'pull-request)
           (when-let ((issue (thing-at-point 'github-issue)))
             (and
              (magithub-issue--issue-is-pull-p issue)
              (magithub-cache :issues
                `(magithub-request
                  (ghubp-get-repos-owner-repo-pulls-number
                      ',(magithub-issue-repo issue)
                      ',issue))))))))

(defun magithub-verify-manage-labels (&optional interactive)
  "Verify the user has permission to manage labels.
If the authenticated user does not have permission, an error will
be signaled.

If INTERACTIVE is non-nil, a `user-error' will be raised instead
of a signal (e.g., for interactive forms)."
  (let-alist (thing-at-point 'github-repository)
    (if .permissions.push t
      (if interactive
          (user-error "You're not allowed to manage labels in %s" .full_name)
        (signal 'error `(unauthorized manage-labels ,(progn .full_name)))))))

(defun magithub-bug-reference-mode-on ()
  "In GitHub repositories, configure `bug-reference-mode'."
  (interactive)
  (when (magithub-usable-p)
    (when-let ((repo (magithub-repo)))
      (bug-reference-mode 1)
      (setq-local bug-reference-bug-regexp "#\\(?2:[0-9]+\\)")
      (setq-local bug-reference-url-format
                  (format "%s/issues/%%s" (alist-get 'html_url repo))))))

(defun magithub-filter-all (funcs list)
  "Return LIST without elements that fail any element of FUNCS."
  (dolist (f funcs)
    (setq list (cl-remove-if-not f list)))
  list)

(defcustom magithub-preferred-remote-method 'ssh_url
  "Preferred method when cloning or adding remotes.
One of the following:

  `clone_url' (https://github.com/octocat/Hello-World.git)
  `git_url'   (git://github.com/octocat/Hello-World.git)
  `ssh_url'   (git@github.com:octocat/Hello-World.git)"
  :group 'magithub
  :type '(choice
          (const :tag "https" clone_url)
          (const :tag "git" git_url)
          (const :tag "ssh" ssh_url)))

(defun magithub-repo--clone-url (repo)
  "Get the preferred cloning URL from REPO."
  (alist-get magithub-preferred-remote-method repo))

(defun magithub--wait-for-git (proc &optional seconds)
  "Wait for git process PROC, polling every SECONDS seconds."
  (let ((seconds (or seconds 0.5)))
    (while (process-live-p proc)
      (sit-for seconds))))

(defmacro magithub--run-git-synchronously (&rest body)
  (declare (debug t))
  (let ((valsym (cl-gensym)) final-form)
    (while body
      (let ((form (pop body)))
        (push `(let ((,valsym ,form))
                 (if (processp ,valsym)
                     (magithub--wait-for-git ,valsym)
                   ,valsym))
              final-form)))
    `(progn
       ,@(nreverse final-form))))

(defun magithub-core-bucket (collection key-func &optional value-func)
  "Bucket COLLECTION by ENTRY-FUNC and VALUE-FUNC.

Each element of COLLECTION is passed through KEY-FUNC to
determine its key in an alist.  If specified, the value is
determined by VALUE-FUNC.

Returns an alist of these keys to lists of values.

See also `magithub-fnnor-each-bucket'."
  (unless value-func
    (setq value-func #'identity))
  (let (bucketed)
    (dolist (item collection)
      (let ((entry (funcall key-func item))
            (val (funcall value-func item)))
        (if-let (bucket (assoc entry bucketed))
            (push val (cdr bucket))
          (push (cons entry (list val))
                bucketed))))
    bucketed))

(defmacro magithub-core-bucket-multi (collection &rest buckets)
  "Chain calls to `magithub-core-bucket'."
  (declare (indent 1))
  (let* ((fnelsym (cl-gensym))
         (apply-to fnelsym)
         form)
    (while buckets
      (setq form `(magithub-core-bucket
                   ,(or form collection)
                   (lambda (,fnelsym) (funcall ,(pop buckets) ,apply-to)))
            apply-to `(car ,apply-to)))
    form))

(defmacro magithub-for-each-bucket (buckets key values &rest body)
  "Do things for each bucket in BUCKETS.

For each bucket in BUCKETs, bind the key to KEY and its
contents (a list) to VALUES and execute BODY.

See also `magithub-core-bucket'."
  (declare (indent 3) (debug t))
  (let ((buckets-sym (cl-gensym)))
    `(let ((,buckets-sym ,buckets))
       (while ,buckets-sym
         (-let (((,key . ,values) (pop ,buckets-sym)))
           ,@body)))))

(defmacro magithub-defsort (symbol compare doc accessor)
  "Define SYMBOL to be a sort over two objects.
COMPARE is used on the application of ACCESSOR to each argument."
  (declare (doc-string 3) (indent 2))
  `(defun ,symbol (a b) ,doc (,(eval compare)
                              (funcall ,accessor a)
                              (funcall ,accessor b))))

(defun magithub-core-color-completing-read (prompt)
  "Generic completing-read for a color."
  (let* ((colors (list-colors-duplicates))
         (len (apply #'max (mapcar (lambda (c) (length (car c))) colors)))
         (sample (make-string 20 ?\ )))
    (car
     (magithub--completing-read
      prompt colors
      (lambda (colors)
        (format (format "%%-%ds  %%s" len) (car colors)
                (propertize sample 'face `(:background ,(car colors)))))))))

(defun magit-section-show-level-5 ()
  "Show surrounding sections up to fifth level."
  (interactive)
  (magit-section-show-level 5))

(defun magit-section-show-level-5-all ()
  "Show all sections up to fifth level."
  (interactive)
  (magit-section-show-level -5))

(defun magithub--refresh-reset ()
  "Reset everything to the defaults after refreshing.
To be added to `magit-unwind-refresh-hook'."
  (setq magithub-cache--refresh nil)
  ;; reclaim some memory
  (setq magithub-cache--refreshed-forms nil))

(defvar magithub-cache--refresh nil
  ;; Can also consider making this a list in the future to refresh
  ;; multiple forms.  No current use-case for this, though.
  "Non-nil when refreshing.
If t, all form classes will be refreshed.  Otherwise, if non-nil,
this variable is expected to be `eq' to the class of forms that
should be selectively refreshed.")

(make-obsolete 'magithub-refresh 'magithub--refresh "0.2")
(defun magithub-refresh ()
  (interactive (user-error (substitute-command-keys
			    "This is no longer an interactive function; \
use \\[universal-argument] \\[magit-refresh] instead :-)"))))

(defun magithub--refresh ()
  "Refresh GitHub data.
Use directly at your own peril; this is intended for use with
`magit-pre-refresh-hook'."
  (when (and current-prefix-arg
             (memq this-command '(magit-refresh
                                  magit-refresh-all
                                  magithub-ci-refresh
                                  magithub-issue-refresh))
             (magithub-usable-p)
             (magithub-confirm-no-error 'refresh)
             (or (magithub--api-available-p)
                 (magithub-confirm-no-error 'refresh-when-API-unresponsive)))
    ;; `magithub--refresh' is part of `magit-pre-refresh-hook' and our requests
    ;; are made as part of `magit-refresh'.  There's no way we can let-bind
    ;; `magithub-settings--refresh' around that entire form, so  we do the next
    ;; best thing: use `magit-unwind-refresh-hook' to reset the  override back
    ;; to its old value.
    (setq magithub-cache--refresh t)
    (setq magithub-cache--refreshed-forms nil)))

(defun magithub-wash-gfm (text)
  "Wash TEXT as it comes from the API."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "" nil t)
      (delete-char -1))
    (s-trim (buffer-string))))

(defun magithub-fill-gfm (text)
  "Fill TEXT according to GFM rules."
  (with-temp-buffer
    (delay-mode-hooks
      (gfm-mode)                        ;autoloaded
      (insert text)
      ;; re font-lock-ensure: see jrblevin/markdown-mode#251
      (font-lock-ensure)
      (fill-region (point-min) (point-max))
      (buffer-string))))

(defun magithub-indent-text (indent text)
  "Indent TEXT by INDENT spaces."
  (replace-regexp-in-string (rx bol) (make-string indent ?\ ) text))

(defun magithub-commit-browse (rev)
  "Browse REV on GitHub.
Interactively, this is the commit at point."
  (interactive (list (or (when-let ((rev (magit-rev-verify
					   (oref (magit-current-section) value))))
                           rev)
                         (thing-at-point 'git-revision))))
  (if-let ((parsed (magit-rev-parse rev)))
      (if-let ((commits (magithub-request
                         (ghubp-get-repos-owner-repo-commits
                             (magithub-repo) nil
                           :sha parsed))))
          (let-alist (car commits)
            (browse-url .html_url))
        (user-error "No commit %s on remote" parsed))
    (error "Could not parse %S" rev)))

(defun magithub-add-thing ()
  "Conceptual command to add a thing (e.g., label, assignee, ...)"
  (interactive)
  (user-error "There is no thing at point that could be added to"))

(defun magithub-browse-thing ()
  "Conceptual command to browse a thing on GitHub"
  (interactive)
  (user-error "There is no thing at point that could be browsed"))

(defun magithub-edit-thing ()
  "Conceptual command to edit a thing (e.g., comment)"
  (interactive)
  (user-error "There is no thing at point that could be edited"))

(defun magithub-reply-thing ()
  "Conceptual command to reply to a thing (e.g., comment)"
  (interactive)
  (user-error "There is no thing at point that could be replied to"))

(defvar magithub-map
  (let ((m (make-sparse-keymap)))
    (define-key m "a" #'magithub-add-thing)
    (define-key m "w" #'magithub-browse-thing)
    (define-key m "e" #'magithub-edit-thing)
    (define-key m "r" #'magithub-reply-thing)
    m)
  "Parent keymap for Magithub sections.")

(defmacro magithub-request (&rest body)
  "Execute BODY authenticating as Magithub."
  (declare (debug t))
  `(ghubp-override-context auth 'magithub
     ,@body))

(defun magithub-debug-section (section)
  (interactive (list (magit-current-section)))
  (pp-eval-expression `(oref ,section value)))

(eval-after-load 'magit
  '(progn
     (dolist (hook '(magit-revision-mode-hook git-commit-setup-hook))
       (add-hook hook #'magithub-bug-reference-mode-on))
     (add-hook 'magit-pre-refresh-hook #'magithub--refresh)
     (add-hook 'magit-unwind-refresh-hook
	       #'magithub--refresh-reset)))

(provide 'magithub-core)
;;; magithub-core.el ends here
