;;; magithub-edit-mode.el --- message-editing mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: multimedia

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

;; Edit generic GitHub (markdown) content.  To be used for comments,
;; issues, pull requests, etc.

;;; Code:

(require 'markdown-mode)
(require 'git-commit)

(defvar magithub-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'magithub-edit-submit)
    (define-key m (kbd "C-c C-k") #'magithub-edit-cancel)
    m)
  "Keymap for `magithub-edit-mode'.")

;;;###autoload
(define-derived-mode magithub-edit-mode gfm-mode "Magithub-Edit"
  "Major mode for editing GitHub issues and pull requests.")

(defvar-local magithub-edit-submit-function nil
  "Populated by SUBMIT in `magithub-edit-new'.")
(defvar-local magithub-edit-cancel-function nil
  "Populated by CANCEL in `magithub-edit-new'.")
(defvar-local magithub-edit-previous-buffer nil
  "The buffer we were in when the edit was initiated.")

(defface magithub-edit-title
  '((t :inherit markdown-header-face-1))
  "Face used for the title in issues and pull requests."
  :group 'magithub-faces)

(defun magithub-edit-submit ()
  "Submit this post.
Uses `magithub-edit-submit-function' to do so."
  (interactive)
  (unless (commandp magithub-edit-submit-function t)
    (error "No submit function defined"))
  (magithub-edit--done magithub-edit-submit-function)
  (magithub-cache-without-cache t
    (magit-refresh-buffer)))

(defun magithub-edit-cancel ()
  "Cancel this post.
Offer to save a draft if the buffer is considered modified, then
call `magithub-edit-cancel-function'."
  (interactive)
  ;; Offer to save the draft
  (if (and (buffer-modified-p)
           ;; don't necessarily want to use `magithub-confirm', here
           ;; this is potentially a very dangerous action
           (y-or-n-p "Save draft? "))
      (save-buffer)
    (set-buffer-modified-p nil))

  ;; If the saved draft is empty, might as well delete it
  (when (and (stringp buffer-file-name)
             (file-readable-p buffer-file-name)
             (string= "" (let ((f buffer-file-name))
                           (with-temp-buffer
                             (insert-file-contents f)
                             (buffer-string)))))
    (magithub-edit-delete-draft))

  (magithub-edit--done magithub-edit-cancel-function))

(defun magithub-edit--done (callback)
  "Cleanup this buffer.
If CALLBACK is a command, call it interactively.  (This will
usually be the SUBMIT or CANCEL commands from
`magithub-edit-new'.)  If that function returns a buffer, switch
to that buffer."
  (let ((nextbuf magithub-edit-previous-buffer))
    (when (commandp callback t)
      (let ((newbuf (save-excursion
                      (call-interactively callback))))
        (when (bufferp newbuf)
          (setq nextbuf newbuf))))
    (set-buffer-modified-p nil)
    (kill-buffer)
    (when nextbuf
      (let ((switch-to-buffer-preserve-window-point t))
        (switch-to-buffer nextbuf)))))

(defun magithub-edit-delete-draft ()
  "Delete the draft for the current edit buffer."
  (when (and (stringp buffer-file-name)
             (file-exists-p buffer-file-name)
             (file-writable-p buffer-file-name))
    (delete-file buffer-file-name magit-delete-by-moving-to-trash)
    (message "Deleted %s" buffer-file-name))
  (set-visited-file-name nil))

(cl-defun magithub-edit-new (buffer-name &key cancel content file header prompt-discard-draft submit template)
  "Generate a new edit buffer called BUFFER-NAME and return it.
'Edit' buffers provide a common interface and handling for
submitting, cancelling, and saving drafts of posts.

CANCEL is a function to use upon \\[magithub-edit-cancel].

CONTENT is initial content for the buffer.  It is considered
novel and the buffer will not be closed without prompting to save
a draft.

FILE is the file to use for drafts of this post.

HEADER is a title to use in the header line of the new buffer.

If PROMPT-DISCARD-DRAFT is non-nil, this function will display
the draft before offering to delete it.  This option is
recommended when using \\[universal-argument] with the command
that calls this function.

SUBMIT is a function to use upon \\[magithub-edit-submit].

TEMPLATE is like CONTENT, but is not considered novel.  We won't
ask to save a draft here if post is cancelled."
  (declare (indent 1))
  (let ((prevbuf (current-buffer))
        (file (and (stringp file)
                   (file-writable-p file)
                   file))
        draft)

    ;; Load the draft
    (setq draft (and (stringp file)
                     (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
    (when (string= draft "")
      (setq draft nil))

    ;; Discard the draft if desired
    (when (and draft prompt-discard-draft)
      (with-current-buffer (get-buffer-create " *draft*")
        (erase-buffer)
        (insert draft)
        (view-buffer-other-window (current-buffer))
        ;; don't necessarily want to use `magithub-confirm', here
        ;; this is potentially a very dangerous action
        (when (yes-or-no-p "Discard this draft? ")
          (setq draft nil)
          (when (file-writable-p file)
            (delete-file file magit-delete-by-moving-to-trash)))
        (kill-buffer (current-buffer))))

    (with-current-buffer (get-buffer-create buffer-name)
      (magithub-edit-mode)

      (setq magithub-edit-previous-buffer prevbuf
            magithub-edit-submit-function submit
            magithub-edit-cancel-function cancel)
      (magit-set-header-line-format
       (substitute-command-keys
        (let ((line "submit: \\[magithub-edit-submit] | cancel: \\[magithub-edit-cancel]"))
          (when header
            (setq line (concat line " | " header)))
          line)))

      (when file
        (let ((orig-name (buffer-name))
              (dir default-directory))
          (set-visited-file-name file)
          (rename-buffer orig-name)
          (cd dir)))

      (cond
       (draft
        (insert draft)
        (set-buffer-modified-p nil)
        (goto-char (point-max))
        (message "Loaded existing draft from %s" file))
       (content
        (insert content)
        (goto-char (point-max))
        (message "Loaded initial content"))
       (template
        (insert template)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (message "Loaded template")))

      (current-buffer))))

(provide 'magithub-edit-mode)
;;; magithub-edit-mode.el ends here
