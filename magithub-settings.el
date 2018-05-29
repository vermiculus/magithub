;;; magithub-settings.el --- repo-specific user settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'magit)

(defconst magithub-settings-section "magithub"
  "This string prefixes all Magithub-related git settings.")
(defconst magithub-settings-prefix "magithub"
  "This string prefixes all Magithub-related git settings.")

(defmacro magithub-settings--simple (popup key variable docstring choices default)
  (declare (indent 3) (doc-string 4))
  (unless (stringp variable)
    (error "VARIABLE must be a string: %S" variable))
  (let* ((variable (concat magithub-settings-section "." variable))
         (Nset (concat "magithub-settings--set-" variable))
         (Nfmt (concat "magithub-settings--format-" variable)))
    (let ((Sset (intern Nset))
          (Sfmt (intern Nfmt))
          (docstring (format "%s\n\nThis is the Git variable %S." docstring variable)))
      `(progn
         (defun ,Sset () ,docstring (interactive)
                (magit--set-popup-variable ,variable ,choices ,default))
         (defun ,(intern Nfmt) () ,(format "See `%s'." Nset)
                (magit--format-popup-variable:choices ,variable ,choices ,default))
         (magit-define-popup-variable ',popup ,key ,variable ',Sset ',Sfmt)
         ,variable))))

(defun magithub-settings--value-or (variable default &optional accessor)
  (declare (indent 2))
  (if (magit-get variable)
      (funcall (or accessor #'magit-get) variable)
    default))

;;;###autoload (autoload 'magithub-settings-popup "magithub-settings" nil t)
(magit-define-popup magithub-settings-popup
  "Popup console for managing Magithub settings."
  'magithub-commands)

(magithub-settings--simple magithub-settings-popup ?e "enabled"
  "Enable/disable all Magithub functionality."
  '("true" "false") "true")

(defun magithub-enabled-p ()
  "Returns non-nil if Magithub content is available."
  (magithub-settings--value-or "magithub.enabled" t
    #'magit-get-boolean))

(magithub-settings--simple magithub-settings-popup ?o "online"
  "Controls whether Magithub is online or offline.

- `true': requests are made to GitHub for missing data
- `false': no requests are made to GitHub

In both cases, when there is data in the cache, that data is
used.  Refresh the buffer with a prefix argument to disregard the
cache while refreshing: \\<magit-mode-map>\\[universal-argument] \\[magit-refresh]"
  '("true" "false") "true")

(defun magithub-online-p ()
  "See `magithub-settings--set-magithub.online'.
Returns the value as t or nil."
  (magithub-settings--value-or "magithub.online" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?s "status.includeStatusHeader"
  "When true, the project status header is included in
`magit-status-headers-hook'."
  '("true" "false") "true")

(defun magithub-settings-include-status-p ()
  "Non-nil if the project status header should be included."
  (magithub-settings--value-or "magithub.status.includeStatusHeader" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?i "status.includeIssuesSection"
  "When true, project issues are included in
`magit-status-sections-hook'."
  '("true" "false") "true")

(defun magithub-settings-include-issues-p ()
  "Non-nil if the issues section should be included."
  (magithub-settings--value-or "magithub.status.includeIssuesSection" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?p "status.includePullRequestsSection"
  "When true, project pull requests are included in
`magit-status-sections-hook'."
  '("true" "false") "true")

(defun magithub-settings-include-pull-requests-p ()
  "Non-nil if the pull requests section should be included."
  (magithub-settings--value-or "magithub.status.includePullRequestsSection" t
    #'magit-get-boolean))


(magithub-settings--simple magithub-settings-popup ?x "contextRemote"
  "Use REMOTE as the proxy.
When set, the proxy is used whenever a GitHub repository is needed."
  (magit-list-remotes) "origin")

(defun magithub-settings-context-remote ()
  "Determine the correct remote to use for issue-tracking."
  (magithub-settings--value-or "magithub.contextRemote" "origin"))

(defvar magithub-confirmation
  ;; todo: future enhancement - could allow prompt message to be a function.
  '((pre-submit-pr                     short "You are about to create a pull request to merge branch `%s' into %s:%s; is this what you wanted to do?")
    (submit-pr                         long  "Are you sure you want to submit this pull request?")
    (submit-pr-from-issue              long  "Are you sure you wish to create a PR based on %s by merging `%s' into `%s'?")
    (pr-allow-maintainers-to-submit    short "Allow maintainers to modify this pull request?")
    (submit-issue                      long  "Are you sure you want to submit this issue?")
    (remove-label                      short "Remove label {%s} from this issue?")
    (add-label                         short "Add label(s) {%s} to %s#%s?")
    (create-repo-as-private            long  "Will this be a private repository?")
    (init-repo-after-create            short "Not inside a Git repository; initialize one here?")
    (fork                              long  "Fork this repository?")
    (fork-create-spinoff               short "Create a spinoff branch?")
    (fork-add-me-as-remote             short "Add %s as a remote in this repository?")
    (fork-set-upstream-to-me           short "Set upstream to %s?")
    (clone                             long  "Clone %s to %s?")
    (clone-fork-set-upstream-to-parent short "This repository appears to be a fork of %s; set upstream to that remote?")
    (clone-fork-set-proxy-to-upstream  short "Use upstream as a proxy for issues, etc.?")
    (clone-open-magit-status           short "%s/%s has finished cloning to %s.  Open?")
    (clone-create-directory            short "%s does not exist.  Create it?")
    (ci-refresh-when-offline           short "Magithub offline; refresh statuses anyway?")
    (refresh                           short "Refresh GitHub data?")
    (refresh-when-API-unresponsive     short "GitHub doesn't seem to be responding, are you sure?")
    (label-save-customized-colors      short "Save customization?")
    (user-email                        short "Email @%s at \"%s\"?")
    (user-email-self                   short "Email yourself?")
    (assignee-add                      long  "Assign '%s' to %s#%d?")
    (assignee-remove                   long  "Remove '%s' from %s#%d?")
    (comment                           short "Submit this comment to %s?")
    (comment-edit                      short "Commit this edit?")
    (comment-delete                    long  "Are you sure you wish to delete this comment?")
    (report-error                      short "%s  Report?  (A bug report will be placed in your clipboard.)"))
  "Alist of actions/decisions to their default behaviors and associated prompts.

These behaviors can be overridden with (man)git-config.

A behavior is one of the following symbols:

  `long'
    use `yes-or-no-p' to confirm each time

  `short'
    use `y-or-n-p' to confirm each time

  `allow'
    always allow action

  `deny'
     always deny action")

(defun magithub-confirm (action &rest prompt-format-args)
  "Confirm ACTION using Git config settings.
See `magithub--confirm'."
  (magithub--confirm action prompt-format-args nil))

(defun magithub-confirm-no-error (action &rest prompt-format-args)
  "Confirm ACTION using Git config settings.
See `magithub--confirm'."
  (magithub--confirm action prompt-format-args t))

(defun magithub-settings--from-confirmation-action (action)
  "Create a magithub.confirm.* setting from ACTION."
  (concat
   magithub-settings-section
   ".confirm."
   (let ((pascal-case (replace-regexp-in-string "-" "" (upcase-initials (symbol-name action)))))
     ;; we have PascalCase, we want camelCase
     (concat (downcase (substring pascal-case 0 1))
             (substring pascal-case 1)))))

(defvar magithub-confirm-y-or-n-p-map
  (let ((m (make-keymap)))
    (define-key m (kbd "C-g") 'quit)    ;don't know how to remap keyboard-quit here
    (define-key m "q" 'quit)
    (define-key m (kbd "C-u") 'cycle)
    (define-key m "y" 'allow)
    (define-key m "n" 'deny)
    m))

(defvar magithub-confirm-yes-or-no-p-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m minibuffer-local-map)
    (define-key m [remap universal-argument] #'magithub--confirm-cycle-set-default-interactive)
    m))

(defvar magithub-confirm--current-cycle nil
  "Control how a response should be saved.
This variable should never be set globally; always let-bind it!

  nil
    Do not save the response

  `local'
    Save response locally

  `global'
    Save response globally")

(defun magithub-confirm-yes-or-no-p (prompt var)
  "Like `yes-or-no-p', but optionally save response to VAR."
  (let ((p (concat prompt (substitute-command-keys " (yes, no, or \\[universal-argument]*) ")))
        magithub-confirm--current-cycle old-cycle done answer changed)
    (while (not done)
      (setq changed (not (eq old-cycle magithub-confirm--current-cycle))
            old-cycle magithub-confirm--current-cycle
            answer (read-from-minibuffer
                    (magithub--confirm-get-prompt-with-cycle
                     p var magithub-confirm--current-cycle)
                    ;; default in what was already entered if the save-behavior changed
                    (when changed answer)
                    magithub-confirm-yes-or-no-p-map nil
                    'yes-or-no-p-history))
      ;; If the user activated `magithub--confirm-cycle-set-default-interactive',
      ;; `magithub-confirm--current-cycle' will have been updated.
      (when (and (eq old-cycle magithub-confirm--current-cycle)
                 (stringp answer))
        (setq answer (downcase (s-trim answer)))
        (if (member answer '("yes" "no"))
            (setq done t)
          (message "Please answer yes or no.  ")
          (sleep-for 2))))
    (when magithub-confirm--current-cycle
      (magithub--confirm-cycle-save-var-value
       var (pcase answer
             ("yes" "allow")
             ("no" "deny"))))
    (string= answer "yes")))

(defun magithub-confirm-y-or-n-p (prompt var)
  "Like `y-or-n-p', but optionally save response to VAR."
  (let ((cursor-in-echo-area t)
        (newprompt (format "%s (y, n, C-u*) " prompt))
        magithub-confirm--current-cycle done answer varval explain)
    (while (not done)
      (setq newprompt
            (if explain
                (format "%s (please answer y or n or use C-u to cycle through and set default answers) " prompt)
              (format "%s (y, n, C-u*) " prompt))
            explain nil
            answer
            (lookup-key magithub-confirm-y-or-n-p-map
                        (vector
                         (read-key (magithub--confirm-get-prompt-with-cycle
                                    newprompt var magithub-confirm--current-cycle)))))
      (pcase answer
        (`quit (keyboard-quit))
        (`cycle (magithub--confirm-cycle-set-default))
        (`allow (setq done t varval "allow"))
        (`deny  (setq done t varval "deny"))
        (_ (setq explain t))))
    (when (stringp varval)
      (magithub--confirm-cycle-save-var-value var varval))
    (eq answer 'allow)))

(defun magithub--confirm-cycle-save-var-value (var val)
  "Save VAR with VAL locally or globally.
See `magithub-confirm--current-cycle'."
  (pcase magithub-confirm--current-cycle
    (`local (magit-set val var))
    (`global (magit-set val "--global" var))))

(defun magithub--confirm-cycle-set-default-interactive ()
  "In `magithub--confirm-yes-or-no-p', update behavior."
  (interactive)
  (magithub--confirm-cycle-set-default)
  (exit-minibuffer))

(defun magithub--confirm-cycle-set-default ()
  (setq magithub-confirm--current-cycle
        (cadr (member magithub-confirm--current-cycle
                      '(nil local global)))))

(defun magithub--confirm-get-prompt-with-cycle (prompt var cycle)
  "Get an appropriate PROMPT associated with VAR for CYCLE.
See `magithub-confirm--current-cycle'."
  (propertize
   (pcase cycle
     (`local (format "%s[and don't ask again: git config %s] " prompt var))
     (`global (format "%s[and don't ask again: git config --global %s] " prompt var))
     (_ prompt))
   'face 'minibuffer-prompt))

(defun magithub--confirm (action prompt-format-args noerror)
  "Confirm ACTION using Git config settings.

When PROMPT-FORMAT-ARGS is non-nil, the prompt piece of ACTION's
confirmation spec is passed through `format' with these
arguments.

Unless NOERROR is non-nil, denying ACTION will result in a user
error to abort the action.

This is like `magit-confirm', but a little more powerful.  It
might belong in Magit, but we'll see how it goes."
  (let ((spec (alist-get action magithub-confirmation))
        var default prompt setting choice)
    (unless spec
      (magithub-error "No confirmation settings for %S" spec))
    (unless (= 2 (length spec))
      (magithub-error "Spec for %S must have 2 members: %S" action spec))
    (setq default (symbol-name (nth 0 spec))
          prompt (nth 1 spec)
          var (magithub-settings--from-confirmation-action action))
    (when prompt-format-args
      (setq prompt (apply #'format prompt prompt-format-args)))
    (when (and (null noerror) (string= "deny" default))
      (magithub-error (format "The default for %S is deny, but this will cause an error" action)))

    (setq setting (magithub-settings--value-or var default))
    (when (and (string= setting "deny")
               (null noerror))
      (let ((raw (magit-git-string "config" "--show-origin" var))
            washed)
        (when (string-match (rx bos (group (+ any)) (+ space) (group (+ any)) eos) raw)
          (setq washed (format "%s => %s"
                               (match-string 1 raw)
                               (match-string 2 raw))))
        (user-error "Abort per %s [%s]" var (or washed raw))))

    (setq choice
          (pcase setting
            ("long" (magithub-confirm-yes-or-no-p prompt var))
            ("short" (magithub-confirm-y-or-n-p prompt var))
            ("allow" t)
            ("deny" nil)))

    (or choice
        (unless noerror
          (user-error "Abort")))))

(defun magithub-confirm-set-default-behavior (action default &optional globally)
  "Set the default behavior of ACTION to DEFAULT.

If GLOBALLY is non-nil, make this configuration apply globally.

See `magithub-confirmation' for valid values of DEFAULT."
  (unless (alist-get action magithub-confirmation)
    (error "Action not defined: %S" action))
  (let* ((var (magithub-settings--from-confirmation-action action))
         (args (list var)))
    (when globally
      (push "--global" args))
    (apply #'magit-set
           (if (memq default '(long short allow deny))
               (symbol-name default)
             (error "Invalid default behavior: %S" default))
           args)
    default))

(provide 'magithub-settings)
;;; magithub-settings.el ends here
