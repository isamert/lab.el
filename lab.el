;;; lab.el --- An Emacs interface for GitLab -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/isamert/lab.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

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

;; An Emacs interface for MPV

;;; Code:

(require 's)
(require 'pp)
(require 'vc-git)
(require 'request)
(require 'memoize)

;; TODO
;; - [ ] lab--notify function


;; Customization:

(defvar lab-token
  nil
  "GitLab token.")

(defvar lab-host
  nil
  "GitLab host.")

(defvar lab-group
  nil
  "GitLab group.")

(defvar lab-after-mr-create-functions
  '()
  "Functions to run after an MR is created.")

(defvar lab-after-mr-mark-ready-functions
  '()
  "Functions to run after an MR is marked ready.")

(defvar lab-browse-url-fn
  #'browse-url
  "Function to open external links.")


;;; Internal variables:

(defvar lab--inspect-buffer-name "*lab inspect*"
  "Buffer name for showing pretty printed results.")


;;; Elisp helpers:

(defun lab-last-item (list)
  "Return the last item of LIST."
  (car (last list)))

(defun lab-alist-completing-read (prompt alist)
  "Like `completing-read' but return value of the selected key in
given ALIST."
  (alist-get
   (completing-read prompt alist)
   alist nil nil #'equal))

;; Taken from transient.el
(defun lab--plist-to-alist (plist)
  (let (alist)
    (while plist
      (push (cons (let* ((symbol (pop plist))
                         (name (symbol-name symbol)))
                    (if (eq (aref name 0) ?:)
                        (intern (substring name 1))
                      symbol))
                  (pop plist))
            alist))
    (nreverse alist)))

(defun lab--inspect-obj (obj)
  "Inspect the given elisp OBJ."
  (interactive)
  (get-buffer-create lab--inspect-buffer-name)
  (let ((print-length nil)
        (print-level nil))
    (pp-display-expression obj lab--inspect-buffer-name))
  (unless (get-buffer-window lab--inspect-buffer-name)
    (switch-to-buffer-other-window lab--inspect-buffer-name)))

(defmacro lab--with-completing-read-exact-order (&rest body)
  "Disable any kind of sorting in completing read."
  `(let ((selectrum-should-sort nil)
         (vertico-sort-function nil))
     ,@body))


;;; Git helpers:

(defun lab-git-current-branch ()
  "Return current branch's name."
  (s-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))

(defun lab-git-get-config (conf)
  "`git config --get CONF' wrapper."
  (thread-last
    (format "git config --get '%s'" conf)
    (shell-command-to-string)
    (s-trim)))

(defun lab-git-user-name ()
  "Return user name."
  (thread-last
    (lab-git-get-config "user.email")
    (s-split "@")
    (car)))

(defun lab-git-remote-homepage ()
  (let ((remote-url (lab-git-get-config "remote.origin.url")))
    (cond
     ((s-contains? "@" remote-url)
      (thread-last
        remote-url
        (s-split "@")
        (nth 1)
        (s-replace ":" "/")
        (s-chop-suffix ".git")
        (s-prepend "https://")))
     (t (s-chop-suffix ".git" remote-url)))))


;;; Git helpers (interactive):

;;;###autoload
(defun lab-git-clone (url dir)
  "Clone URL to DIR."
  (interactive
   (list
    (read-string "URL: ")
    (read-directory-name "Directory to clone in: " "~/Workspace/")))
  (let* ((default-directory dir)
         (proc (start-process-shell-command "*lab-clone*" "*lab-clone*" "git clone '%s'")))
    (set-process-sentinel
     proc
     (lambda (p e)
       (if (= 0 (process-exit-status p))
           (message "%s cloned." url)
         (message "Cloning %s is failed." url))))))

;;;###autoload
(defun lab-git-origin-switch-to-ssh ()
  (interactive)
  (when-let* ((https-origin (s-trim (shell-command-to-string "git config --get remote.origin.url")))
              (it (s-match "https://\\(.*\\)\\.\\(com\\|net\\|org\\)/\\(.*\\)" https-origin))
              (ssh-origin (format "git@%s.%s:%s" (nth 1 it) (nth 2 it) (nth 3 it))))
    (shell-command-to-string (format "git remote set-url origin %s" ssh-origin))))


;;; GitLab:

(defun lab--plist-remove-keys-with-prefix (prefix lst)
  (interactive)
  (seq-each
   (lambda (it) (setq lst (map-delete lst it)))
   (seq-filter
    (lambda (it) (s-prefix? prefix (symbol-name it)))
    (map-keys lst)))
  lst)

(defun lab--project-path ()
  "Return hexified project path for current git project.
This is mostly used while doing an api call for the current
project."
  (thread-last
    (lab-git-remote-homepage)
    (s-chop-prefix lab-host)
    (s-chop-prefix "/")
    (s-chop-prefix "/")
    (s-chop-prefix "/")
    (s-trim)
    (url-hexify-string)))

(cl-defun lab--request
    (endpoint
     &rest params
     &key (%type "GET") (%headers '()) (%data nil) (%collect-all? nil)
     &allow-other-keys)
  "Do a GitLab request.
When %COLLECT-ALL is non-nil, do a paged request and collect all
results in a list and return them."

  ;; Remove meta items from params list so that we can use `params' as
  ;; url parameters
  (setq params (lab--plist-remove-keys-with-prefix ":%" params))

  (let (json
        (allitems '())
        (lastid t)
        (per-page 100)
        (json-object-type 'alist)
        (json-array-type #'list)
        (json-key-type 'symbol))
    (while lastid
      (request
        (thread-last (format "%s/api/v4/%s" lab-host endpoint)
                     (s-replace "#{group}" (url-hexify-string lab-group))
                     (s-replace "#{project}" (lab--project-path)))
        :type %type
        :headers `((Authorization . ,(format "Bearer %s" lab-token)) ,@%headers)
        :parser #'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq json data)
                    (when %collect-all?
                      (setq allitems `(,@allitems ,@json)))))
        :sync t
        :data (lab--plist-to-alist %data)
        :params `(,@(when %collect-all?
                      `(("per_page" . ,per-page)
                        ("order_by" . "id")
                        ("sort" . "asc")
                        ("pagination" . "keyset")))
                  ,@(when (and %collect-all? (not (eq lastid t)))
                      `(("id_after" . ,lastid)))
                  ,@(lab--plist-to-alist params)))
      (setq lastid
            (when (and %collect-all? (length= json per-page))
              (alist-get 'id (lab-last-item json)))))
    (if %collect-all? allitems json)))

(defun lab-list-current-branch-merge-requests ()
  "List all open MRs that the source branch is the current branch."
  (interactive)
  (lab--select-mr-and-apply-action
   (lab--request
    "projects/#{project}/merge_requests"
    :scope 'all
    :state 'opened
    :source_branch (lab-git-current-branch))))

(defun lab-list-my-merge-requests ()
  "List all of my currently open merge requests.
`mine' means either it's created by me or assigned to me."
  (interactive)
  (lab--select-mr-and-apply-action
   `(,@(lab--request
        "merge_requests"
        :scope 'created_by_me
        :state 'opened)
     ,@(lab--request
        "merge_requests"
        :scope 'assigned_to_me
        :state 'opened))))

(defun lab-list-group-merge-requests (&optional group)
  "List all open MRs that belongs to GROUP.
If GROUP is omitted, `lab-group' is used."
  (interactive)
  (lab--select-mr-and-apply-action
   (lab--request
    (format "groups/%s/merge_requests" (or group "#{group}"))
    :scope 'all
    :state 'opened)))

(defun lab-list-project-merge-requests (&optional project)
  "List all open MRs that belongs to PROJECT.
If it's omitted,currently open project is used."
  (interactive)
  (lab--select-mr-and-apply-action
   (lab--request
    (format "projects/%s/merge_requests" (or project "#{project}"))
    :scope 'all)))

(defun lab-create-merge-request (&optional project)
  "Create an MR interactively for PROJECT."
  (interactive)
  (let ((result
         (lab--request
          (format "projects/%s/merge_requests" (or project "#{project}"))
          :%type "POST"
          :%data (list :source_branch (completing-read
                                       "Source branch: "
                                       (vc-git-branches)
                                       nil nil (lab-git-current-branch))
                       :target_branch (completing-read
                                       "Target branch: "
                                       (vc-git-branches)
                                       nil nil "master")
                       :title (read-string "MR Title: "
                                           (s-trim (shell-command-to-string "git log -1 --pretty=%B")))
                       ;; TODO Get description (and maybe title) from a dedicated buffer like how magit commit
                       ;; asks input from a user. I already  implemented something like this,
                       ;; see `isamert/get-input'
                       :description (read-string "Description: " (format "Closes %s" (lab-git-current-branch)))
                       :remove_source_branch t))))
    (seq-each
     (lambda (it) (funcall it result))
     lab-after-mr-create-functions)
    (lab-open-web-url result)))

(defun lab-open-web-url (it)
  (let ((url (alist-get 'web_url it)))
    (kill-new url)
    (browse-url (alist-get 'web_url it))))

(defun lab-rebase-merge-request (it)
  (lab--request
   (format "projects/%s/merge_requests/%s/rebase" (alist-get 'project_id it) (alist-get 'iid it))
   :%type "PUT"))

(defun lab-mark-mr-as-ready (mr)
  (let ((result
         (lab--request
          (format "projects/%s/merge_requests/%s" (alist-get 'project_id mr) (alist-get 'iid mr))
          :%type "PUT"
          :%data (list :title (s-chop-prefixes '("WIP: " "Draft: ") (alist-get 'title mr))))))
    (seq-each
     (lambda (it) (funcall it result))
     lab-after-mr-mark-ready-functions)))

(defmemoize lab-get-all-projects ()
  (lab--request
   "groups/#{group}/projects"
   :with_shared 'false
   :include_subgroups 'true
   :per_page 100
   :%collect-all? t))

;;;###autoload
(defun lab-select-project ()
  (interactive)
  (lab--select-project-and-apply-action
   (lab-get-all-projects)))

;;;###autoload
(defun lab-watch-pipeline (url &optional rerun?)
  "Start watching pipeline URL status.
Send a notification if it's finished, failed or waiting for a
manual action."
  (interactive "sPipeline URL: ")
  (let* ((data (s-match "https://.*\\.com/\\(.*\\)/-/pipelines/\\([0-9]+\\)" url))
         (project (nth 1 data))
         (project-hexified (url-hexify-string project))
         (pipeline (nth 2 data)))
    (when (or (not pipeline) (s-blank? pipeline))
      (user-error "Pipeline id is nil for %s" url))
    (unless rerun?
      (message ">> Started watching pipeline %s on %s!" pipeline project))
    (run-with-timer
     (if rerun? 20 1)
     nil
     (lambda ()
       (let-alist (lab--request (format "projects/%s/pipelines/%s" project-hexified pipeline))
         (pcase .status
           ("success"
            (alert (format "Pipeline FINISHED: %s/%s" project pipeline)))
           ("failed"
            (alert (format "Pipeline FAILED: %s/%s" project pipeline)))
           ((or "canceled" "skipped" "scheduled")
            (alert (format "Pipeline %s: %s/%s" (s-upcase .status) project pipeline)))
           ("manual"
            (alert (format "Pipeline requires MANUAL action: %s/%s" project pipeline)))
           (_
            (lab-watch-pipeline url t))))))))


;;; GitLab Internal

(defun lab--mr-format-title (mr)
  (format "[%-15s | %-6s => %-15s] %s"
          (propertize
           (thread-last
             (alist-get 'web_url mr)
             (s-chop-prefix lab-host)
             (s-split "/-/merge_requests/")
             (car)
             (s-split "/")
             (lab-last-item)
             (s-truncate 15))
           'face '(:weight thin))
          (propertize (alist-get 'state mr)
                      'face '(:weight thin :slant italic))
          (propertize (s-truncate 15 (alist-get 'name (alist-get 'author mr)))
                      'face 'italic)
          (propertize (alist-get 'title mr)
                      'face 'bold)))

(defun lab--select-mr-and-apply-action (mrs)
  (thread-last
    mrs
    (seq-map (lambda (it) (cons (lab--mr-format-title it) it)))
    (lab-alist-completing-read "Select MR: ")
    (lab--merge-request-actions)))

(defun lab--merge-request-actions (mr)
  (lab--with-completing-read-exact-order
   (let ((action (completing-read
                  "Select an action: "
                  '("Open page" "Copy url" "Rebase" "Mark as ready" "Raw"))))
     (pcase action
       ("Open page" (lab-open-web-url mr))
       ("Copy url" (kill-new (alist-get 'web_url mr)))
       ("Mark as ready" (lab-mark-mr-as-ready mr))
       ("Rebase" (lab-rebase-merge-request mr))
       ("Raw" (lab--inspect-obj mr))))))

(defun lab--select-project-and-apply-action (projects)
  (let* ((project-alist (seq-map
                         (lambda (it) (cons (alist-get 'name_with_namespace it) it))
                         projects))
         (selected (lab-alist-completing-read
                    "Project: "
                    project-alist))
         (action (completing-read "Act: " '("Clone" "Open" "List merge requests"))))
    (let-alist selected
      (pcase action
        ("Clone"
         (lab-git-clone
          .ssh_url_to_repo
          (read-directory-name "Dir to clone in: " "~/Workspace/")))
        ("Open" (funcall lab-browse-url-fn .web_url))
        ("List merge requests" (lab-list-project-merge-requests .id))))))

(provide 'lab)

;;; lab.el ends here
