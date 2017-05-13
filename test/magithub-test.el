;;; magithub-tests.el --- tests for Magithub

;; Copyright (C) 2016  Sean Allred
;;
;; License: GPLv3

;;; Code:

(require 'ert)



(require 'magithub-cache)
(ert-deftest magithub-test-cache ()
  (should (equal (magithub-cache-value 1 :test)
                 nil))
  (should (equal (magithub-cache 1 :test '(list 1 2 3))
                 '(1 2 3)))
  (should (equal (magithub-cache-value 1 :test)
                 '(1 2 3)))
  (should (equal (magithub-cache-value 2 :test)
                 nil))
  (should (equal (magithub-cache 2 :test '(list 2 4 6))
                 '(2 4 6)))
  (should (equal (magithub-cache-value 2 :test)
                 '(2 4 6)))
  (should (equal (magithub-cache-value 2 :test-another)
                 nil))
  (should (equal (magithub-cache 2 :test-another 100)
                 100))
  (should (equal (magithub-cache-value 2 :test-another)
                 100)))

;;; magithub-test.el ends here
