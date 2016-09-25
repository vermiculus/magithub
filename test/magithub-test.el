;;; magithub-tests.el --- tests for Magithub

;; Copyright (C) 2016  Sean Allred
;;
;; License: GPLv3

;;; Code:

(require 'ert)

(add-to-list 'load-path ".")

(ert-deftest magithub-test-compile-core ()
  (should (byte-compile-file "magithub-core.el")))
(ert-deftest magithub-test-compile-issue ()
  (should (byte-compile-file "magithub-issue.el")))
(ert-deftest magithub-test-compile-ci ()
  (should (byte-compile-file "magithub-ci.el")))
(ert-deftest magithub-test-compile-main ()
  (should (byte-compile-file "magithub.el")))

;;; magithub-test.el ends here
