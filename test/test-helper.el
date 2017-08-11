;;; Allow loading package files
(add-to-list 'load-path default-directory)

(load-file "./test/magithub-test-compile.el")

(setq ghub-username (getenv "GITHUB_USER")
      ghub-token (getenv "GITHUB_TOKEN")
      ghub-authenticate (and ghub-username ghub-token))
