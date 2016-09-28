;;; magithub-cache.el --- caching network data       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

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

(defvar magithub-cache--cache (make-hash-table)
  "A hash table to use as a cache.
Entries are of the form (time-entered . value).")

(defvar magithub-cache-refresh-seconds-plist
  (list :issues 600 :ci-status 60)
  "The number of seconds that have to pass for GitHub data to be
considered outdated.")

(defun magithub-cache (cache default)
  "The cached value for CACHE (set to DEFAULT if necessary)."
  (declare (indent defun))
  (let ((cached-value (gethash cache magithub-cache--cache :no-value)))
    (if (or (eq cached-value :no-value)
            (< (plist-get magithub-cache-refresh-seconds-plist cache)
               (time-to-seconds (time-since (car cached-value)))))
        (cdr (puthash cache (cons (current-time) (eval default))
                      magithub-cache--cache))
      (when magithub-debug-mode
        (message "Using cached value for %S (retrieved %s)"
                 cache (format-time-string "%F %T" (car cached-value))))
      (cdr cached-value))))

(defun magithub-cache-value (cache)
  "The cached value for CACHE."
  (let ((c (gethash cache magithub-cache--cache :no-value)))
    (unless (eq c :no-value)
      (cdr c))))

(defun magithub-cache-clear (&optional cache)
  "Clear the cache for CACHE.
If CACHE is nil, the entire cache is cleared."
  (if cache (remhash cache magithub-cache--cache)
    (setq magithub-cache--cache (make-hash-table))))

(defun magithub-refresh ()
  "Refresh all GitHub data."
  (interactive)
  (magithub-cache-clear)
  (magit-refresh))

(provide 'magithub-cache)
;;; magithub-cache.el ends here
