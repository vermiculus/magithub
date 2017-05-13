;;; magithub-tests.el --- tests for Magithub

;; Copyright (C) 2016  Sean Allred
;;
;; License: GPLv3

;;; Code:

(require 'ert)

(defmacro magithub-test-cache-with-new-cache (plist &rest body)
  (declare (indent 1))
  `(let ((magithub-cache-class-refresh-seconds-alist ',plist)
         (magithub-cache--cache (make-hash-table)))
     ,@body))

(require 'magithub-cache)
(ert-deftest magithub-test-cache ()
  (magithub-test-cache-with-new-cache ((:test . 30))
    (should (equal t (magithub-cache :test t)))))

;;; magithub-test.el ends here
