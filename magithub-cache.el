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

(require 'magithub-core)

(defvar magithub-cache-class-refresh-seconds-alist
  '((:issues . 600)
    (:ci-status . 60)
    (t . 60))
  "The number of seconds that have to pass for GitHub data to be
considered outdated.

If no valid entry is found for ")

(defvar magithub-cache--cache
  (make-hash-table :test 'equal))

(defun magithub-cache--expired-p (saved-time class)
  (let ((a magithub-cache-class-refresh-seconds-alist))
    (or (null a)
        (< (or (alist-get class a)
               (alist-get t a 0))
           (time-to-seconds (time-since saved-time))))))

(defun magithub-cache (expiry-class form &optional message)
  "The cached value for CACHE (set to (eval DEFAULT) if necessary)."
  (declare (indent defun))
  (let* ((not-there (cl-gensym))
         (cache (cons (magithub-source-repo) form))
         (cached-value (gethash cache magithub-cache--cache not-there)))
    (cdr
     (if (or (eq cached-value not-there)
             (magithub-cache--expired-p (car cached-value) expiry-class))
         (let ((current-time (current-time))
               (v (with-temp-message message
                    (eval form))))
           (puthash cache (cons current-time v) magithub-cache--cache))
       (when magithub-debug-mode
         (message "Using cached value for %S (retrieved %s)"
                  cache (format-time-string "%F %T" (car cached-value))))
       cached-value))))

(defun magithub-cache-invalidate ()
  "Clear the cache"
  (let ((r (magithub-source-repo)))
    (maphash
     (lambda (k v)
       (when (equal r (car k))
         (remhash k magithub-cache--cache)))
     magithub-cache--cache)))

(defun magithub-refresh ()
  "Refresh all GitHub data."
  (interactive)
  (magithub-cache-invalidate)
  (magit-refresh))

(provide 'magithub-cache)
;;; magithub-cache.el ends here
