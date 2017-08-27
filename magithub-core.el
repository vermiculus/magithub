;;; magithub-core.el --- core functions for magithub  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Sean Allred

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
(require 'dash)
(require 's)
(require 'subr-x)
(require 'ghub)
(require 'bug-reference)
(require 'cl-lib)

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
  (magithub-debug-message
   "ghub-request%S" `(,(car args)
                      ,(concat ghub-base-url (cadr args))
                      ,@(cddr args)))
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

;;; Turning Magithub on/off
(defun magithub-enable ()
  "Enable Magithub for this repository."
  (interactive)
  (magit-set "yes" "magithub" "enabled")
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh))
  (message "Magithub is now enabled in this repository"))

(defmacro magithub-in-data-dir (&rest forms)
  "Execute forms in `magithub-dir'.
If `magithub-dir' does not yet exist, it and its parents will be
created automatically."
  `(progn
     (unless (file-directory-p magithub-dir)
       (mkdir magithub-dir t))
     (let ((default-directory magithub-dir))
       ,@forms)))

(defun magithub-disable ()
  "Disable Magithub for this repository."
  (interactive)
  (magit-set "no" "magithub" "enabled")
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh))
  (message "Magithub is now disabled in this repository"))

(defun magithub-enabled-p ()
  "Returns non-nil when Magithub is enabled for this repository."
  (and (member (magit-get "magithub" "enabled") '("yes" nil)) t))

(defun magithub-enabled-toggle ()
  "Toggle Magithub integration."
  (interactive)
  (if (magithub-enabled-p)
      (magithub-disable)
    (magithub-enable)))

;;; Caching; Online/Offline mode
(defvar magithub-cache 'expire
  "Determines how the cache behaves.

If nil, the cache will not be used to read cached data.  It will
still be updated and written to disk.

If t, *only* the cache will be used.  This constitutes Magithub's
'offline' mode.

If `expire', the cache will expire with the passage of time
according to `magithub-cache-class-refresh-seconds-alist'.  This
is the default behavior.

A fourth value, `hard-refresh-offline', counts towards both
`magithub-offline-p' and `magithub-cache--always-p'.  It should
only be let-bound by `magithub-refresh'.")

(defun magithub-go-offline (&optional no-refresh)
  "Take Magithub offline.
No API requests will be made; all information displayed will be
retrieved from the cache."
  (interactive)
  (setq magithub-cache t)
  (unless no-refresh
    (when (derived-mode-p 'magit-status-mode)
      (magit-refresh)))
  (message "Magithub is now offline everywhere"))

(defun magithub-go-online ()
  "Take Magithub online.
API requests will be made to refresh expired caches."
  (interactive)
  (setq magithub-cache 'expire)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh))
  (message "Magithub is now online everywhere"))

(defun magithub-toggle-offline ()
  "See `magithub-go-online' and `magithub-go-offline'.
Uses `magithub-offline-p' as the check."
  (interactive)
  (if (magithub-offline-p)
      (magithub-go-online)
    (magithub-go-offline)))

(defun magithub-offline-p ()
  "Non-nil if Magithub is not supposed to make API requests."
  (memq magithub-cache '(t hard-refresh-offline)))

(defun magithub-cache--always-eval-p ()
  "True if the cache should always re-evaluate its source forms."
  (memq magithub-cache '(nil hard-refresh-offline)))
(defun magithub-cache--never-eval-p  ()
  "True if the cache should never re-evaluate its source forms."
  (eq magithub-cache t))

(defcustom magithub-cache-file "cache"
  "Use this file for Magithub's persistent cache."
  :group 'magithub
  :type 'file)

(defvar magithub-cache-class-refresh-seconds-alist
  '((:issues . 600)
    (:ci-status . 15)
    (:repo-demographics . 86400)
    (:user-demographics . 86400)
    (:label . 3600))
  "The maximum age of each type of information.
The number of seconds that have to pass for GitHub data to be
considered outdated.")

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

(defun magithub-cache-read-from-disk ()
  "Returns the cache as read from disk `magithub-cache-file'."
  (when (file-readable-p magithub-cache-file)
    (with-temp-buffer
      (insert-file-contents magithub-cache-file)
      (read (current-buffer)))))

(defun magithub-cache--expired-p (saved-time class &optional default)
  "True if a cached value has expired.

* CLASS is the class of the cached value.
* SAVED-TIME is when the cached value was last computed.

If `magithub-cache-class-refresh-seconds-alist' does not contain
a expiry time for CLASS, DEFAULT is used."
  (let ((a magithub-cache-class-refresh-seconds-alist))
    (or (null a)
        (< (or (alist-get class a)
               (alist-get t a (or default 0)))
           (time-to-seconds (time-since saved-time))))))

(cl-defun magithub-cache (expiry-class form
                                       &optional message
                                       &key (context 'repo))
  "The cached value for FORM if available.

If FORM has not been cached or its EXPIRY-CLASS dictates the
cache has expired, FORM will be re-evaluated.

MESSAGE may be specified for intensive functions.  We'll display
this with `with-temp-message' while the form is evaluating.

CONTEXT is a symbol specifying the cache context.  If it's the
special symbol `repo', we'll use the context of the current
repository."
  (declare (indent defun))

  (when (eq context 'repo)
    (setq context (magithub-source--sparse-repo)))

  (let* ((not-there (cl-gensym))
         (cached-value (gethash (cons context form) magithub-cache--cache not-there)))
    (cdr
     (if (and (not (magithub-cache--never-eval-p))
              (or (magithub-cache--always-eval-p)
                  (eq cached-value not-there)
                  (magithub-cache--expired-p (car cached-value) expiry-class)))
         (let ((current-time (current-time))
               (v (with-temp-message
                      (if (magithub-debug-mode 'forms)
                          (let ((print-quoted t))
                            (format "%s -- %S" message form))
                        message)
                    (eval form))))
           (prog1 (puthash (cons context form) (cons current-time v) magithub-cache--cache)
             (setq magithub-cache--needs-write t)
             (run-with-idle-timer 10 nil #'magithub-cache-write-to-disk)))
       (unless (eq not-there cached-value)
         (when (magithub-debug-mode 'forms)
           (magithub-debug-message "using cached value for form: %S" form))
         cached-value)))))

(defun magithub-cache-invalidate ()
  "Clear the cache from memory."
  (maphash
   (lambda (k _)
     (remhash k magithub-cache--cache))
   magithub-cache--cache))

(defun magithub-cache-invalidate--confirm ()
  "True if the user really does want to invalidate the cache."
  (yes-or-no-p
   (concat (if (magithub--api-available-p 'ignore-offline-mode) "Are"
             "GitHub doesn't seem to be responding; are")
           " you sure you want to refresh all GitHub data? ")))

(defun magithub-refresh (&optional force)
  "Refresh all GitHub data.  With a prefix argument, invalidate cache."
  (interactive "P")
  (setq force (and force t))           ; force `force' to be a boolean
  (unless (or (not force)
              (magithub--api-available-p 'ignore-offline-mode))
    (user-error "Aborting"))
  (let* ((offline (magithub-offline-p))
         (magithub-cache (cond
                          ((and force offline) 'hard-refresh-offline)
                          (force nil)
                          (offline t)
                          (t 'expire))))
    (when force
      (unless (magithub-cache-invalidate--confirm)
        (user-error "Aborting"))
      (magithub-cache-invalidate))
    (magit-refresh)))

(defun magithub-maybe-report-offline-mode ()
  "Conditionally inserts the OFFLINE header.

If this is a Magithub-enabled repository and we're offline, we
insert a header notifying the user that all data shown is cached.
To aid in determining if the cache should be refreshed, we report
the age of the oldest cached information."
  (when (and (magithub-usable-p)
             (magithub-offline-p))
    (magit-insert-section (magithub)
      (insert
       (propertize
        (concat
         "Magithub is "
         (propertize
          "OFFLINE"
          'face 'font-lock-warning-face
          'help-echo "test")
         "; you are seeing cached data"
         (if-let ((oldest (car (magithub-cache--age
                                (magithub-repo)))))
             (format "%s (%s ago)"
                     (format-time-string " as old as %D %r" oldest)
                     (magithub-cache--time-out
                      (time-subtract (current-time) oldest)))
           ;; if the above is nil, that means the cache is empty.  If
           ;; the cache is empty and we're about to print the
           ;; magit-status buffer, we're probably going to have cached
           ;; information by the time we finish showing the buffer
           ;; (after which the user will see this message).
           " (nothing cached)"))
        'help-echo
        (substitute-command-keys
         "To update a section anyway, place point on the section and use C-u \\[magit-refresh]"))))))

;;; If we're offline, display this at the top
(eval-after-load "magit"
  '(add-hook 'magit-status-headers-hook
             #'magithub-maybe-report-offline-mode))

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

(defun magithub-cache--age (&optional repo)
  "Retrieve the oldest and newest times present in the cache.

If REPO is non-nil, it is a repo object (as returned by
`magithub-source--repo' and results will be filtered to that
repository context."
  (setq repo (magithub--repo-simplify repo))
  (let (times)
    (maphash (lambda (k v) (when (or (null repo) (equal (car k) repo))
                             (push (car v) times)))
             magithub-cache--cache)
    (when times
      (setq times (sort times #'time-less-p))
      (cons (car times) (car (last times))))))

(defun magithub-cache-write-to-disk ()
  "Write the cache to disk.
The cache is writtin to `magithub-cache-file' in
`magithub-data-dir'"
  (maphash
   (lambda (k v)
     (when (magithub-cache--expired-p
            (car v) :pre-write-trim 86400)
       (remhash k magithub-cache--cache)))
   magithub-cache--cache)
  (if (active-minibuffer-window)
      (run-with-idle-timer 10 nil #'magithub-cache-write-to-disk) ;defer
    (when magithub-cache--needs-write
      (magithub-in-data-dir
       (with-temp-buffer
         (insert (prin1-to-string magithub-cache--cache))
         (write-file magithub-cache-file)))
      (setq magithub-cache--needs-write nil)
      (magithub-debug-message "wrote cache to disk"))))

(defmacro magithub-cache-without-cache (class &rest body)
  "For CLASS, execute BODY without using CLASS's caches."
  (declare (indent 1))
  `(let ((magithub-cache-class-refresh-seconds-alist
          (cons (cons ,class 0)
                magithub-cache-class-refresh-seconds-alist)))
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

(defun magithub--api-available-p (&optional ignore-offline-mode)
  "Non-nil if the API is available.
Pings the API a maximum of once every ten seconds."
  (when (magithub-enabled-p)
    (unless (and (not ignore-offline-mode) (magithub-offline-p))
      (magithub-debug-message "checking if the API is available")
      (prog1
          (when
              (condition-case _
                  (progn
                    (magithub-debug-message "making sure authinfo is unlocked")
                    (ghub--token))
                ;; Magithub only works when authenticated.
                (ghub-auth-error
                 (prog1 nil
                   (if (y-or-n-p "Not yet authenticated; open instructions in your browser? ")
                       (progn
                         (browse-url "https://github.com/magit/ghub#initial-configuration")
                         (setq magithub--api-offline-reason "Try again once you've authenticated"))
                     (setq magithub--api-offline-reason "Not yet authenticated per ghub's README")))))
            (if (and magithub--api-last-checked
                     (< (time-to-seconds (time-since magithub--api-last-checked)) 10))
                (prog1 magithub--api-last-checked
                  (magithub-debug-message "used cached value for api-last-checked"))

              (magithub-debug-message "cache expired; retrieving new value for api-last-checked")
              (setq magithub--api-last-checked (current-time))

              (let (api-status error-data response)
                (condition-case err
                    (progn
                      (setq response
                            (condition-case _
                                (with-timeout (magithub-api-timeout
                                               (signal 'magithub-api-timeout nil))
                                  (ghub-get "/rate_limit"))

                              (ghub-404
                               ;; Rate-limiting is often disabled on
                               ;; Enterprise instances.  Try using /meta
                               ;; which should (hopefully) always work.  See
                               ;; also issue #107.
                               (ghub-get "/meta")))
                            api-status (and response t))

                      (magithub-debug-message "new value retrieved for api-last-available: %S"
                                              response))

                  ;; Sometimes, the API can take a long time to respond
                  ;; (whether that's GitHub not responding or requests being
                  ;; blocked by some client-side firewal).  Handle this
                  ;; possiblity gracefully.
                  (magithub-api-timeout
                   (setq error-data err
                         magithub--api-offline-reason
                         (concat "API is not responding quickly; "
                                 "consider customizing `magithub-api-timeout' if this happens often")))

                  ;; Never hurts to be cautious :-)
                  (error
                   (setq error-data err
                         magithub--api-offline-reason (format "unknown issue: %S" err))))

                (when error-data
                  (magithub-debug-message "consider reporting unknown error while checking api-available: %S"
                                          error-data))

                api-status)))
        (when magithub--api-offline-reason
          (magithub-go-offline 'no-refresh)
          (run-with-idle-timer 2 nil #'magithub--api-offline-reason))))))

(defun magithub--api-offline-reason ()
  "Report the reason we're going offline and go offline.
Refresh the status buffer if necessary.

See `magithub--api-offline-reason'."
  (when magithub--api-offline-reason
    (message "Magithub is now offline: %s"
             magithub--api-offline-reason)
    (setq magithub--api-offline-reason nil)
    (magithub-go-offline)))

;;; Repository parsing
(defun magithub-github-repository-p ()
  "Non-nil if \"origin\" points to GitHub or a whitelisted domain."
  (when-let ((origin (magit-get "remote" "origin" "url")))
    (-some? (lambda (domain) (s-contains? domain origin))
            (cons "github.com" (magit-get-all "hub" "host")))))

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
                     (group (+? (| alnum "-" "."))) ;owner.login -- vermiculus
                     "/"
                     (group (+? (| alnum "-" "."))) ;name -- magithub
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
                 ;; git+ssh://github.com/vermiculus/magithub.git
                 (rx bol
                     (or (seq "http" (? "s"))
                         (seq "git" (? "+ssh")))
                     "://"
                     (group (+? any)) ;domain -- github.com
                     "/"
                     (group (+? (| alnum "-" "."))) ;owner.login -- vermiculus
                     "/"
                     (group (+? (| alnum "-" "."))) ;name -- magithub
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

(defun magithub-source--remote ()
  "Tries to determine the correct remote to use for issue-tracking."
  (or (magit-get "magithub" "proxy") "origin"))

(defun magithub-source--sparse-repo ()
  "Returns the sparse repository object for the current context.

Only information that can be determined without API calls will be
included in the returned object."
  (magithub-repo-from-remote--sparse
   (magithub-source--remote)))

(defun magithub-repo-from-remote (remote)
  (magithub-repo (magithub-repo-from-remote--sparse remote)))

(defun magithub-repo-from-remote--sparse (remote)
  (magithub--url->repo (magit-get "remote" remote "url")))

(defalias 'magithub-source-repo 'magithub-repo)
(make-obsolete 'magithub-source-repo 'magithub-repo "0.1.4")
(defun magithub-repo (&optional sparse-repo)
  "Turn SPARSE-REPO into a full repository object.
If SPARSE-REPO is null, the current context is used."
  (let ((sparse-repo (or sparse-repo (magithub-source--sparse-repo))))
    (magithub-cache :repo-demographics
      `(condition-case e
           (or (ghubp-get-repos-owner-repo ',sparse-repo)
               (and (not magithub--api-available-p)
                    sparse-repo))
         (ghub-404
          ;; Repo may not exist; ignore 404
          nil))
      nil
      :context nil)))

;;; Repository utilities
(defun magithub-repo-name (repo)
  (let-alist repo
    (if .full_name .full_name
      (concat .owner.login "/" .name))))

(defun magithub--repo-simplify (repo)
  "Convert full repository object REPO to a sparse repository object."
  (let (login name)
    ;; There are syntax problems with things like `,.owner.login'
    (let-alist repo
      (setq login .owner.login
            name .name))
    `((owner (login . ,login))
      (name . ,name))))

;;; Feature checking
(defconst magithub-feature-list
  '(pull-request-merge pull-request-checkout)
  "All magit-integration features of Magithub.

`pull-request-merge'
Apply patches from pull request

`pull-request-checkout'
Checkout pull requests as new branches")

(defvar magithub-features nil
  "An alist of feature-symbols to Booleans.
When a feature symbol maps to non-nil, that feature is considered
'loaded'.  Thus, to disable all messages, prepend '(t . t) to
this list.

Example:

    ((pull-request-merge . t) (other-feature . nil))

signals that `pull-request-merge' is a loaded feature and
`other-feature' has not been loaded and will not be loaded.

To enable all features, see `magithub-feature-autoinject'.

See `magithub-feature-list' for a list and description of features.")

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

(defun magithub-error (err-message tag &optional trace)
  "Report a Magithub error."
  (setq trace (or trace (with-output-to-string (backtrace))))
  (when (y-or-n-p (concat tag "  Report?  (A bug report will be placed in your clipboard.)"))
    (with-current-buffer-window
     (get-buffer-create "*magithub issue*")
     #'display-buffer-pop-up-window nil
     (when (fboundp 'markdown-mode) (markdown-mode))
     (insert
      (kill-new
       (format
        "## Automated error report

### Description

%s

### Backtrace

```
%s```
"
        (read-string "Briefly describe what you were doing: ")
        trace))))
    (magithub--meta-new-issue))
  (error err-message))

;;; Miscellaneous utilities

(defun magithub--completing-read (prompt collection &optional format-function predicate require-match default)
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

(defun magithub--completing-read-multiple (prompt collection &optional format-function predicate require-match default)
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
       (or (and (magithub-offline-p)
                ;; if we're offline, source-repo will get the cached value
                (magithub-repo))
           (and (magithub--api-available-p)
                ;; otherwise, we only want to query the API if it's available
                (magithub-repo)))))

(defmacro magithub--deftoggle (name hook func s)
  "Define a section-toggle command."
  (declare (indent defun))
  `(defun ,name ()
     ,(concat "Toggle the " s " section.")
     (interactive)
     (if (memq ,func ,hook)
         (remove-hook ',hook ,func)
       (add-hook ',hook ,func t))
     (when (derived-mode-p 'magit-status-mode)
       (magit-refresh))
     (memq ,func ,hook)))

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

(defconst magithub--object-text-prop-prefix
  "magithub-object-"
  "Prefix used for text properties.
Used for `magithub-thing-at-point' and related functions.")

(defun magithub--object-text-prop (type)
  "Returns a text property symbol for TYPE."
  (intern (concat magithub--object-text-prop-prefix (symbol-name type))))
(defun magithub--object-text-prop-inv (prop)
  "Returns the type referred to by the text property symbol PROP."
  (intern (substring (symbol-name prop) (length magithub--object-text-prop-prefix))))
(defun magithub--object-text-prop-p (prop)
  "Returns non-nil if PROP is a Magithub object text property."
  (s-prefix-p magithub--object-text-prop-prefix (symbol-name prop)))

(defun magithub--object-propertize (type object text)
  "Gives a type-TYPE OBJECT property to TEXT."
  (declare (indent 2))
  (propertize text (magithub--object-text-prop type) object))

(defun magithub-thing-at-point (type)
  "Determine the thing of TYPE at point.
If TYPE is `all', an alist of types to objects is returned."
  (if (eq type 'all)
      (let ((plist (text-properties-at (point))) alist)
        (while plist
          (when (magithub--object-text-prop-p (car plist))
            (thread-first (car plist)
              (magithub--object-text-prop-inv)
              (cons (cadr plist))
              (push alist)))
          (setq plist (cddr plist)))
        alist)
    (get-text-property (point) (magithub--object-text-prop type))))

(defun magithub-get-in-all (props object-list)
  "Follow property-path PROPS in OBJECT-LIST.
Returns a list of the property-values."
  (declare (indent 1))
  (if (or (null props) (not (consp props)))
      object-list
    (magithub-get-in-all (cdr props)
      (mapcar (lambda (o) (alist-get (car props) o))
              object-list))))

(defun magithub-verify-manage-labels (&optional interactive)
  "Verify the user has permission to manage labels.
If the authenticated user does not have permission, an error will
be signaled.

If INTERACTIVE is non-nil, a `user-error' will be raised instead
of a signal (e.g., for interactive forms)."
  (let-alist (magithub-repo)
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

(defvar magithub-preferred-remote-method 'ssh_url
  "Preferred method when cloning or adding remotes.
One of the following:

  `clone_url' (https://github.com/octocat/Hello-World.git)
  `git_url'   (git:github.com/octocat/Hello-World.git)
  `ssh_url'   (git@github.com:octocat/Hello-World.git)")

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

(eval-after-load "magit"
  '(dolist (hook '(magit-revision-mode-hook git-commit-setup-hook))
     (add-hook hook #'magithub-bug-reference-mode-on)))

(provide 'magithub-core)

;;; magithub-core.el ends here
