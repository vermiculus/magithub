;;; Allow loading package files
(add-to-list 'load-path default-directory)

;;; Avoid awkward situations where Travis has to answer a question
;;; https://travis-ci.org/vermiculus/magithub/jobs/231915489#L880
(let ((d (expand-file-name "magithub" user-emacs-directory)))
  (or (file-exists-p d) (mkdir d)))

(load-file "./test/magithub-test-compile.el")
