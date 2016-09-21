;;; magithub-tests.el --- tests for Magithub

;; Copyright (C) 2016  Sean Allred
;;
;; License: GPLv3

;;; Code:

(require 'ert)

(add-to-list 'load-path ".")

(ert-deftest magithub-test-compile ()
  (should (byte-compile-file "magithub.el"))
  (should (byte-compile-file "magithub-issue.el")))

;;; magithub-test.el ends here
