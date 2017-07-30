;;; Allow loading package files
(add-to-list 'load-path default-directory)

(load-file "./test/magithub-test-compile.el")

;;; Don't authenticate these requests; no known way to keep that
;;; authentication secret
(setq ghub-authenticate nil)
