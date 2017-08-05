;;; magithub-cache.el --- caching network data       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: data, tools

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

(require 'cl-lib)

(require 'magithub-core)

(defun magithub-cache--always-eval-p () (memq magithub-cache '(nil hard-refresh-offline)))
(defun magithub-cache--never-eval-p  () (eq magithub-cache t))

(defcustom magithub-cache-file
  "cache"
  "Use this file for Magithub's persistent cache.
Unless it's absolute, this filename is relative inside
`magithub-dir'."
  :group 'magithub
  :type 'file)

(defvar magithub-cache-class-refresh-seconds-alist
  '((:issues . 600)
    (:ci-status . 15)
    (:repo-demographics . 86400)
    (:user-demographics . 86400)
    (:label . 3600))
  "The number of seconds that have to pass for GitHub data to be
considered outdated.")

(defvar magithub-cache--cache
  (or (ignore-errors
        (magithub-cache-read-from-disk))
      (make-hash-table :test 'equal)))
(defvar magithub-cache--needs-write nil
  "when non-nil, the cache will be written to disk next time the
idle timer runs")

(defun magithub-cache-read-from-disk ()
  (when (file-readable-p magithub-cache-file)
    (with-temp-buffer
      (insert-file-contents magithub-cache-file)
      (read (current-buffer)))))

(defun magithub-cache--expired-p (saved-time class &optional default)
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
  "Clear the cache"
  (maphash
   (lambda (k _)
     (remhash k magithub-cache--cache))
   magithub-cache--cache))

(defun magithub-cache-invalidate--confirm ()
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
                                (magithub-source-repo)))))
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

;;; If we're offline, display this at the top
(add-hook 'magit-status-headers-hook
          #'magithub-maybe-report-offline-mode)

(add-hook 'kill-emacs-hook
          #'magithub-cache-write-to-disk)

(provide 'magithub-cache)
;;; magithub-cache.el ends here
