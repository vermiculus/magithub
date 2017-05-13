(require 'magit)
(require 'magithub-issue)

(defun magithub-pull-request-checkout (pull-request)
  "Checkout PULL-REQUEST as a local branch."
  (interactive (list (magithub-issue-completing-read-pull-requests)))
  (unless (executable-find magithub-hub-executable)
    (user-error "This hasn't been supported in elisp yet; please install/configure `hub'"))
  (let-alist pull-request
    (magithub-with-hub
     (magit-checkout .html_url))
    (dolist (var-val (list (cons "URL" .html_utl)
                           (cons "ID"  .number)))
      (magit-set (cdr var-val)
                 "branch" (magit-get-current-branch)
                 (concat "magithubPullRequest" (car var-val))))))

(defun magithub-pull-request-merge (pull-request &optional args)
  "Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default."
  (interactive (list (magithub-issue-completing-read-pull-requests)
                     (magit-am-arguments)))
  (unless (executable-find magithub-hub-executable)
    (user-error "This hasn't been supported in elisp yet; please install/configure `hub'"))
  (unless (member pull-request (magithub-pull-requests))
    (user-error "Unknown pull request %S" pull-request))
  (magithub-with-hub
   (magit-run-git-sequencer "am" args (plist-get pull-request :url)))
  (message "#%d has been applied" (plist-get pull-request :number)))

(provide 'magithub-issue-tricks)
