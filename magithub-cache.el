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

(defvar magithub-offline-mode nil)
(defvar magithub-cache-file
  (expand-file-name "cache" magithub-dir))

(defvar magithub-cache-class-refresh-seconds-alist
  '((:issues . 600)
    (:ci-status . 60))
  "The number of seconds that have to pass for GitHub data to be
considered outdated.

If no valid entry is found for ")

(defvar magithub-cache--cache
  (or (ignore-errors
        (magithub-cache-read-from-disk))
      (make-hash-table :test 'equal)))

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

(defun magithub-cache (expiry-class form &optional message)
  "The cached value for CACHE (set to (eval DEFAULT) if necessary)."
  (declare (indent defun))
  (let* ((not-there (cl-gensym))
         (cache (cons (magithub-source-repo) form))
         (cached-value (gethash cache magithub-cache--cache not-there)))
    (cdr
     (if (and (not magithub-offline-mode)
              (or (eq cached-value not-there)
                  (magithub-cache--expired-p (car cached-value) expiry-class)))
         (let ((current-time (current-time))
               (v (with-temp-message message
                    (eval form))))
           (prog1 (puthash cache (cons current-time v) magithub-cache--cache)
             (run-with-idle-timer 10 nil #'magithub-cache-write-to-disk)))
       (unless (eq not-there cached-value)
         cached-value)))))

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

(defun magithub-maybe-report-offline-mode ()
  (when (and (magithub-usable-p)
             magithub-offline-mode)
    (magit-insert-section (magithub)
      (insert
       "Magithub is "
       (propertize
        "OFFLINE"
        'face 'font-lock-warning-face)
       "; you are seeing cached data"))))

(defun magithub-go-offline ()
  (interactive)
  (setq magithub-offline-mode t)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))
(defun magithub-go-online ()
  (interactive)
  (setq magithub-offline-mode nil)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defun magithub-cache-write-to-disk ()
  (maphash
   (lambda (k v)
     (when (magithub-cache--expired-p
            (car v) :pre-write-trim 86400)
       (remhash k magithub-cache--cache)))
   magithub-cache--cache)
  (with-temp-buffer
    (insert (prin1-to-string magithub-cache--cache))
    (write-file magithub-cache-file))
  (message "Magithub: wrote cache to disk"))

;;; If we're offline, display this at the top
(add-hook 'magit-status-headers-hook
          #'magithub-maybe-report-offline-mode)

(provide 'magithub-cache)
;;; magithub-cache.el ends here
