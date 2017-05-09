(require 'magit)
(require 'magithub-issue)

(defun magithub-pull-request-checkout (pull-request)
  "Checkout PULL-REQUEST as a local branch."
  (interactive (list (magithub-pull-request--completing-read)))
  (let-alist pull-request
    (when-let (((url .html_url) (num .number)))
      (magithub-with-hub
       (magit-checkout url))
      (dolist (var-val `(("URL" . ,url)
                         ("ID" . ,num)))
        (magit-set (cdr var-val)
                   "branch" (magit-get-current-branch)
                   (concat "magithubPullRequest" (car var-val)))))))

(defun magithub-pull-request-merge (pull-request &optional args)
  "Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default."
  (interactive (list (magithub-pull-request--completing-read)
                     (magit-am-arguments)))
  (unless (member pull-request (magithub-pull-requests))
    (user-error "Unknown pull request %S" pull-request))
  (magithub-with-hub
   (magit-run-git-sequencer "am" args (plist-get pull-request :url)))
  (message "#%d has been applied" (plist-get pull-request :number)))

(provide 'magithub-issue-tricks)
