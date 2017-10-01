;;; Allow loading package files
(add-to-list 'load-path default-directory)

(defun magithub-in-test-dir (file)
  "Expand FILE in the test directory."
  (let ((dir default-directory))
    (while (and (not (string= dir "/"))
                (not (file-exists-p (expand-file-name ".git" dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (when (string= dir "/")
      (error "Project root not found"))
    (setq dir (expand-file-name "test" dir))
    (expand-file-name file dir)))

(load-file (magithub-in-test-dir "magithub-test-compile.el"))

(defun magithub-mock-ghub-request (method resource &optional params data)
  "Mock a call to the GitHub API.

If the call has not been mocked and the AUTOTEST environment
variable is not set, offer to save a snapshot of the real API's
response."
  (message "(mock-ghub-request %S %S %S %S)" method resource params data)
  (when (eq t magithub-cache)
    (error "Did not respect cache"))
  (let* ((parts (cdr (s-split "/" resource)))
         (directory (mapconcat (lambda (s) (concat s ".d"))
                               (butlast parts) "/"))
         (filename (magithub-in-test-dir (format "mock-data/%s/%s/%s"
                                                 (downcase method)
                                                 directory
                                                 (car (last parts))))))
    (if (file-readable-p filename)
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (read (current-buffer)))
      (if (and (not (getenv "AUTOTEST"))
               (y-or-n-p (format "Request not mocked; mock now?")))
          (progn
            (make-directory directory t)
            (let ((real-data (ghub-request method resource params data)))
              (pp-display-expression real-data "*GitHub API Response*")
              (if (y-or-n-p "API response displayed; is this ok?")
                  (with-temp-buffer
                    (insert (pp-to-string real-data))
                    (write-file filename)
                    (message "Wrote %s" filename))
                (error "API response rejected"))))
        (error "Unmocked test: %S %S" method resource)))))
