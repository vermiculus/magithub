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
             (should (byte-compile-file ,f))))
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

(ignore                                 ;
 ' (let ((default-directory (magit-toplevel)))
     (require 'f)
     (mapcar #'file-name-nondirectory
             (f-files default-directory
                      (lambda (f)
                        (and (string= "el" (file-name-extension f))
                             (magit-git-success "ls-files" f "--error-unmatch")))))))
