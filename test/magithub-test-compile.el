(require 'ert)

(defmacro magithub-test-compile (&rest files)
  `(progn
     ,@(mapcar
        (lambda (f)
          `(ert-deftest
               ,(intern
                 (format "magithub-test-compile--%s"
                         (file-name-base f)))
               ()
             (should (let ((byte-compile-error-on-warn t))
                       (byte-compile-file ,f)))))
        files)))

(magithub-test-compile
 "magithub-cache.el"
 "magithub-ci.el"
 "magithub-core.el"
 "magithub-issue-post.el"
 "magithub-issue-status.el"
 "magithub-issue-tricks.el"
 "magithub-issue.el"
 "magithub-label.el"
 "magithub-proxy.el"
 "magithub.el")
