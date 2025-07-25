;;; lab.el --- An interface for GitLab -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 2.4.0
;; Homepage: https://github.com/isamert/lab.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (request "0.3.2") (s "1.10.0") (f "0.20.0") (compat "29.1.4.4") (promise "1.1") (async-await "1.1"))

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

;; lab.el is an Emacs package that provides a simple integration with
;; GitLab (managed or self-hosted).
;;
;; Provides you easier access to some GitLab functionality regarding to:
;; - Projects :: search, list, open, clone, list MRs...
;; - Merge Requests :: list, open, create, rebase, list pipelines...
;; - Pipelines :: list, open, retry, cancel, delete, watch status...
;; - Jobs :: list, open, retry, cancel, delete, show logs/traces...
;;
;; lab.el also provides you `lab--request' function which you can use
;; to do GitLab API calls easily.

;;; Code:

(require 'compat)
(require 's)
(require 'pp)
(require 'vc)
(require 'f)
(require 'vc-git)
(require 'request)
(require 'ansi-color)
(require 'async-await)
(require 'promise)
(require 'auth-source)

;; Customization:

(defgroup lab nil
  "An interface for GitLab."
  :group 'tools
  :prefix "lab-")

(defcustom lab-token
  nil
  "GitLab API token."
  :type 'string
  :group 'lab)

(defcustom lab-host
  "https://gitlab.com"
  "GitLab host, like `https://gitlab.mycompany.com'."
  :type 'string
  :group 'lab)

(defcustom lab-group
  nil
  "The GitLab group you mostly work on.
Required only for functions containing `-group-' phrase."
  :type 'string
  :group 'lab)

(defcustom lab-after-merge-requests-create-functions
  '()
  "Functions to run after an MR is created."
  :type 'hook
  :group 'lab)

(defcustom lab-after-merge-request-mark-ready-functions
  '()
  "Functions to run after an MR is marked ready."
  :type 'hook
  :group 'lab)

(defcustom lab-after-git-clone-functions
  '()
  "Functions to run after a repository is cloned.
The `default-directory' will be the project's directory while
calling these functions."
  :type 'hook
  :group 'lab)

(defcustom lab-browse-url-fn
  #'browse-url
  "Function to open external links."
  :type 'function
  :group 'lab)

(defcustom lab-projects-directory
  (expand-file-name "~")
  "Default place where local projects are stored."
  :type 'directory
  :group 'lab)

(defcustom lab-result-count
  20
  "Total result count for `lab-list-X' functions.
Can't be higher than `lab--max-per-page-result-count'."
  :type 'integer
  :group 'lab)

(defcustom lab-should-open-pipeline-on-manual-action?
  nil
  "Should pipeline web page automatically be opened if it requires a manual action?"
  :type 'boolean
  :group 'lab)

(defcustom lab-clone-method
  'ssh
  "Prefered for cloning repositories into your local."
  :type '(choice (const :tag "SSH" ssh)
                 (const :tag "HTTPS" https))
  :group 'lab)

(defcustom lab-action-handler
  'read-multiple-choice
  "Default action handler.
`lab' uses the given function for listing possible actions on a
selection."
  :type '(choice (const :tag "completing-read" completing-read)
                 (const :tag "read-multiple-choice" read-multiple-choice))
  :group 'lab)

(defcustom lab-main-branch-name
  '("main" "master")
  "Default main branch name used in your projects, like \"main\", \"master\" etc.
It also can be a list, like \\='(\"main\" \"master\") (which is the default).
 Mostly used for convenience-related stuff."
  :type 'string
  :group 'lab)

(defcustom lab-default-merge-request-create-options
  (list
   ;; Premium feature:
   ;; :approvals_before_merge t
   ;; My broke-ass yaml parser does not support arrays right now, so:
   ;; :labels '()
   :squash "false"
   :allow_collaboration t
   :remove_source_branch t)
  "Default options used while creating a merge request."
  :group 'lab
  :type 'sexp)

(defcustom lab-pipeline-watcher-initial-delay
  30
  "Initial delay before starting to watch pipelines.
Pipelines do not start immediately after a push, it may take a
while for them to come online.  This delay is used in functions
`lab-watch-pipeline-for-*' so that you can use these functions as
hooks without worrying about lags.  Value is in seconds."
  :type 'number
  :group 'lab)

(defcustom lab-pipeline-watcher-debounce-time 30
  "Pipeline status is checked by polling.
This is the duration between calls, in seconds."
  :type 'number
  :group 'lab)

(defcustom lab-pipeline-automatically-watch-after-retry t
  "Whether to start watching pipeline after retrying a pipeline job."
  :type 'number
  :group 'lab)

(defcustom lab-pull-bulk-switch-to-main nil
  "Whether to switch to main branch before pulling in `lab-pull-bulk'.
If set to a non-nil value, then switch to the main branch of the
repository and then pull project.  See `lab-main-branch-name' to
learn how the \"main\" branch is determined."
  :group 'lab
  :type 'boolean)

(defcustom lab-use-consult-if-possible t
  "Use `consult' for some flows, like project search.
Setting this variable to t does not affect anything if you
haven't installed consult."
  :group 'lab
  :type 'boolean)

;;;; Internal variables/constants:

(defvar lab--inspect-buffer-name "*lab inspect*"
  "Buffer name for showing pretty printed results.")

(defvar lab--action-selection-title "Action: "
  "The text displayed on action selection menus.")

(defconst lab--max-per-page-result-count 100
  "This is the hard limit set by GitLab.")

(defconst lab--regex-yaml-metadata-border
  "\\(-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata.")

(defconst lab--diff-buffer-name "*lab-diff*")

(defconst lab--git-clone-buffer-name "*lab-clone*")

(defconst lab--pull-bulk-buffer "*lab-pull-bulk*")

(defvar lab--interrupt nil)

(defconst lab--trigger-pipeline-user-input-helper-text
  "# Specify variable values to be used in this run.
# Each line should contain a variable assignment like:
#   SOME_VAR=true
#
# Comments lines like this are skipped.
")

(defvar lab--user-input-history (make-hash-table :test #'equal)
  "History for user inputs.")

;;;; Public variables & utilities

;;;###autoload
(defvar lab-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'lab-git-clone)

    (define-key map "pl" #'lab-act-on-last-failed-pipeline-job)
    (define-key map "pp" #'lab-list-project-pipelines)
    (define-key map "pg" #'lab-list-all-group-projects)
    (define-key map "po" #'lab-list-all-owned-projects)

    (define-key map "wp" #'lab-watch-pipeline)
    (define-key map "wc" #'lab-watch-pipeline-for-last-commit)

    (define-key map "mc" #'lab-create-merge-request)
    (define-key map "mm" #'lab-list-my-merge-requests)
    (define-key map "mg" #'lab-list-group-merge-requests)
    (define-key map "mb" #'lab-list-branch-merge-requests)
    (define-key map "mp" #'lab-list-project-merge-requests)
    map)
  "Keymap for commonly used interactive lab functions.
It is not bound to any key by default.  You can bind this keymap
to a key, like following:

  (bind-key \"C-x l\" lab-map)

...and now you can do `C-x mm' to list your open merge requests,
for example.  Do \\[execute-extended-command] `describe-keymap'
lab-map to list all actions in this keymap.")

(defun lab-interrupt ()
  "Interrupt currently long-running `lab' process."
  (interactive)
  (setq lab--interrupt t))

;;;; Elisp helpers:

(defun lab-last-item (list)
  "Return the last item of LIST."
  (car (last list)))

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
  (get-buffer-create lab--inspect-buffer-name)
  (let ((print-length nil)
        (print-level nil))
    (pp-display-expression obj lab--inspect-buffer-name))
  (unless (get-buffer-window lab--inspect-buffer-name)
    (switch-to-buffer-other-window lab--inspect-buffer-name)))

(defun lab--plist-remove-keys-with-prefix (prefix lst)
  "Return a copy of LST without the key-value pairs whose keys start with PREFIX."
  (seq-each
   (lambda (it) (setq lst (map-delete lst it)))
   (seq-filter
    (lambda (it) (s-prefix? prefix (symbol-name it)))
    (map-keys lst)))
  lst)

(defun lab--length= (lst it)
  (= (length lst) it))

(defun lab--alist-path-get (path alist)
  "Get the value associated with a specific PATH in ALIST.

>> (let ((alist `((a . ((b . ((c . d)))))))
         (path `(a b c)))
    (lab--alist-path-get path alist))
=> d"
  (if (eq (length path) 1)
      (alist-get (car path) alist)
    (lab--alist-path-get (seq-drop path 1) (alist-get (car path) alist))))

;;;; Utilities:

;; The discussion made here[1] was quite helpful for implementing the following functionality.
;; [1]: https://github.com/oantolin/embark/issues/495

(cl-defun lab--completing-read-object (prompt objects &key (formatter #'identity) category predicate require-match initial-input hist def inherit-input-method (sort? t))
  "`completing-read' with formatter and sort control.
Applies FORMATTER to every object in OBJECTS and propertizes
candidates with the actual object so that they can be retrieved
later by embark actions.  Also adds category metadata to each
candidate, if given.  PROMPT passed to `completing-read' as is."
  (let* ((object-table
          (make-hash-table :test 'equal :size (length objects)))
         (object-strings
          (mapcar
           (lambda (object)
             (let ((formatted-object (funcall formatter object)))
               (puthash formatted-object object object-table)
               (propertize formatted-object 'lab--completing-read-object object)))
           objects))
         (selected
          (completing-read
           prompt
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   ,(when category (cons 'category category))
                   ,@(unless sort?
                       '((display-sort-function . identity)
                         (cycle-sort-function . identity))))
               (complete-with-action
                action object-strings string predicate)))
           predicate require-match initial-input hist def inherit-input-method)))
    (gethash selected object-table selected)))

(cl-defun lab--user-input
    (&key
     (mode #'fundemental-mode)
     (init "")
     (buffer-name "*lab-input*")
     (on-start #'ignore)
     on-accept
     (on-reject  #'ignore)
     history-key
     parser)
  "Prompt the user for input in a new buffer with customizable options.
Keyword arguments:
- MODE - the major mode to use for the new buffer.
- INIT - initial text to display in the buffer.
- BUFFER-NAME - name of the new buffer to create.
- ON-START - no-arg function to run before the user input prompt is displayed.
- ON-ACCEPT - function to run when the user accepts the input.
- ON-REJECT - no-arg function to run when the user rejects the input
- PARSER - an function to parse user input before passing to on-accept function
- HISTORY-KEY - key used to store and retrieve input history in the variable
  `lab--user-input-history'

The prompt provides instructions for the user to accept or reject
input.  When the user accepts the input, the on-accept function
is called with the input and parser result if given, and the
buffer is killed.  When the user rejects the input, the on-reject
function is called if given and the buffer is simply killed."
  (let* ((source-buffer (current-buffer))
         (buffer (get-buffer-create buffer-name))
         (success-handler (lambda ()
                            (interactive)
                            (let ((parser-result (when parser
                                                   (with-current-buffer buffer
                                                     (funcall parser))))
                                  (result (substring-no-properties (buffer-string))))
                              (kill-buffer buffer)
                              (puthash history-key result lab--user-input-history)
                              (with-current-buffer source-buffer
                                (if parser
                                    (funcall on-accept result parser-result)
                                  (funcall on-accept result))))))
         (reject-handler (lambda ()
                           (interactive)
                           (kill-buffer buffer)
                           (when on-reject
                             (funcall on-reject)))))
    (with-current-buffer buffer
      (funcall mode)
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key (kbd "C-c C-c") success-handler)
      (local-set-key (kbd "C-c C-k") reject-handler)
      (setq header-line-format "Hit `C-c C-c' to save `C-c C-k' to reject.")
      (insert (gethash history-key lab--user-input-history init))
      (funcall on-start)
      (switch-to-buffer-other-window buffer))))

(defun lab--extract-object-from-target (type target)
  (cons type (or (get-text-property 0 'lab--completing-read-object target) target)))

(defvar embark-keymap-alist)
(defvar embark-transformer-alist)
(defvar embark-general-map)
(cl-defmacro lab--define-actions-for (category &key formatter keymap sort?)
  (declare (indent 1))
  (let ((lab--generate-action-name
         (lambda (category name &optional public?)
           (intern (format "lab%s%s-%s" (if public? "-" "--") category (s-replace " " "-" (downcase name))))))
        (lab-keymap-full-name (intern (format "lab--embark-keymap-for-%s" category)))
        (lab-category-full-name (intern (format "lab-%s" category))))
    `(progn
       ;; Generate non-interactive action definitions for this
       ;; category (simple defuns). When user selects an action, one
       ;; of these functions are dispatched. These are non-interactive
       ;; because embark does not support passing objects to actions
       ;; if they are interactive.
       ,@(mapcar
          (lambda (keydef)
            (let ((action (seq-subseq keydef 2))
                  (name (nth 1 keydef)))
              (when (listp action)
                `(defun ,(funcall lab--generate-action-name category name) (it)
                   (let-alist it
                     ,@action)))))
          keymap)

       ;; Define embark keymap and add it to `embark-keymap-alist.'
       ;; This uses the functions generated above.
       (with-eval-after-load 'embark
         (defvar-keymap ,lab-keymap-full-name
           :doc ,(format "Actions for %s" lab-category-full-name)
           :parent embark-general-map
           ;; Define the first binding as the default action
           "RET" #',(funcall lab--generate-action-name category (nth 1 (car keymap)))
           ,@(apply
              #'append
              (mapcar
               (lambda (keydef)
                 `(,(char-to-string (nth 0 keydef)) #',(funcall lab--generate-action-name category (nth 1 keydef))))
               keymap)))

         (add-to-list 'embark-keymap-alist '(,lab-category-full-name . ,lab-keymap-full-name))

         ;; Also define a embark transformer for these actions. That's
         ;; because we want to pass full objects to the actions instead
         ;; of passing selected string. Objects are attached to the
         ;; text-properties by `lab--completing-read-object'
         ;; function. This extractor simply extracts it.
         (setf
          (alist-get ',lab-category-full-name embark-transformer-alist)
          #'lab--extract-object-from-target))

       ;; Generate the `...-act' function which let's user select one
       ;; of the inputs and act on them
       ;; TODO make interactive?
       (cl-defun ,(funcall lab--generate-action-name category "act-on" t) (item)
         (let* ((action (pcase lab-action-handler
                          ('read-multiple-choice
                           (nth 1 (read-multiple-choice
                                   lab--action-selection-title
                                   ',(mapcar
                                      (lambda (keydef)
                                        (let ((name (downcase (nth 1 keydef))))
                                          (list (nth 0 keydef) name)))
                                      keymap))))
                          ('completing-read
                           (completing-read
                            lab--action-selection-title
                            ',(mapcar (lambda (keydef) (nth 1 keydef)) keymap)))))
                (action-fn (,lab--generate-action-name ',category action)))
           (funcall action-fn item)))

       (cl-defun ,(funcall lab--generate-action-name category "select-and-act-on" t) (items)
         (ignore-error (quit minibuffer-quit)
           (let* ((result (lab--completing-read-object
                           (format "%s: " (s-titleize (format "%s" ',category)))
                           items
                           :formatter ,formatter
                           :category ',lab-category-full-name
                           :sort? ,sort?)))
             (funcall #',(funcall lab--generate-action-name category "act-on" t) result)))))))

(defvar project-current-inhibit-prompt)
(defvar project-current-directory-override)
(defmacro lab--within-current-project (&rest forms)
  "Let you run FORMS in current projects directory."
  `(let ((default-directory
           (or
            (when (boundp 'project-current-inhibit-prompt)
              project-current-inhibit-prompt)
            (when (boundp 'project-current-directory-override)
              project-current-directory-override)
            default-directory)))
     ,@forms))

(defun lab--path-join (p1 &rest rest)
  "Join P1 and REST into a single path.

>> (lab--path-join \"/home/isa\" \"x/y\" \"z.json\")
=> \"/home/isa/x/y/z.json\"
>> (lab--path-join \"/home/isa/\" \"x/y/\" \"z.json\")
=> \"/home/isa/x/y/z.json\""
  (seq-reduce (lambda (acc it) (concat acc "/" (string-remove-suffix "/" it))) rest (string-remove-suffix "/" p1)))

(defun lab--listify (obj)
  (if (listp obj)
      obj
    (list obj)))

(defvar project-prompter)

;; TODO: Maybe let users customize this? Right now users can only
;; select a project through `project-prompter' but maybe they want to
;; select a project that is not on their local.
(defun lab--read-project-id ()
  "Select a project interactively and return it's project id."
  (let ((default-directory (funcall project-prompter)))
    (lab--project-path)))

(defun lab--read-project-id-interactive-helper ()
  "Return current project id or let user select a project and return its id.
If `current-prefix-arg' is non-nil, force select a project first without
checking current project.  Otherwise check if current project is a real
GitLab project and then if so, return it's project id, else, ask user to
select a project first."
  (if current-prefix-arg
      (lab--read-project-id)
    (if-let ((project-id (lab--project-path t)))
        project-id
      (lab--read-project-id))))

(defconst lab--text-property-prefix "lab-")

(defun lab--with-text-properties (str &rest props)
  (let ((str-copy (copy-sequence str)))
    (add-text-properties
     0 1
     (seq-mapcat
      #'identity
      (seq-map
       (lambda (prop)
         (cons (intern (concat lab--text-property-prefix
                               (string-trim-left (symbol-name (car prop)) ":")))
               (cdr prop)))
       (seq-partition props 2)))
     str-copy)
    str-copy))

(defun lab--get-text-property (str prop)
  (get-text-property
   0
   (intern (concat lab--text-property-prefix
                   (string-trim-left (symbol-name prop) ":")))
   str))

;;;; Consult integration

(declare-function consult--read "consult")
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-refresh "consult")
(declare-function consult--async-indicator "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--async-split "consult")

(defun lab--consult-async-generator (request mapper)
  (lambda (next)
    (lambda (action)
      (pcase action
        ((pred stringp)
         (when (not (string-empty-p (string-trim action)))
           (funcall
            request
            action
            (lambda (result)
              (funcall next 'flush)
              (funcall next (funcall mapper result))))))
        (_ (funcall next action))))))

(defun lab--consult-async-wrapper (async)
  (consult--async-pipeline
   (consult--async-split)
   (consult--async-throttle)
   async
   (consult--async-indicator)
   (consult--async-refresh)))

(defun lab--use-consult? ()
  (and lab-use-consult-if-possible (require 'consult nil t)))

;;;; Private git utilities

(defun lab--git (cmd &rest options)
  "Run git CMD with OPTIONS.
This returns a promise.  When the git CMD fails, the promise is
rejected otherwise resolved value contains the output of the git
CMD."
  (promise-new
   (lambda (resolve reject)
     (let* ((pname (format "*lab-git-%s*" cmd))
            (proc (start-process-shell-command
                   pname nil
                   (format "git --no-pager %s %s" cmd (string-join options " "))))
            (output ""))
       (set-process-filter
        proc
        (lambda (_process out)
          (setq output (concat output out))))
       (set-process-sentinel
        proc
        (lambda (p _e)
          (if (= 0 (process-exit-status p))
              (funcall resolve (string-trim output))
            (funcall reject (string-trim output)))))))))

(async-defun lab--pull-bulk-single (repository)
  "Do a git pull in the REPOSITORY.
This is specifically designed for `lab-pull-bulk' and have no
other uses."
  (condition-case reason
      (let* ((default-directory repository)
             (branches (split-string (await (lab--git "branch" "--list")) "\n" t "[ \\*\t]+"))
             (current-branch (await (lab--git "branch" "--show-current")))
             ;; NOTE: I didn't use `lab--find-main-branch' here as I
             ;; need an async way of getting it instead of blocking
             (main-branch (seq-find
                           (lambda (branch)
                             (string-match (regexp-opt (lab--listify lab-main-branch-name) t) (or branch "NULL")))
                           branches))
             (needs-checkout? (not (equal current-branch main-branch))))
        (await (lab--git "stash"))
        (when (and lab-pull-bulk-switch-to-main needs-checkout?)
          (await (lab--git "checkout" main-branch)))
        (list 'success (await (lab--git "pull"))))
    (error
     (list 'error (cadr reason)))))

;;;; Project helpers:

;;;###autoload
(defun lab-all-project-roots (&optional dir)
  "Find every project dir under DIR.
DIR is `lab-projects-directory' by default.

This function simply checks for folders with `.git' under them."
  (thread-last
    (expand-file-name (or dir lab-projects-directory))
    (format "fd . \"%s\" --type directory --maxdepth 6 --absolute-path")
    (shell-command-to-string)
    (string-trim)
    (s-split "\n")
    (seq-filter #'lab-git-dir?)
    (seq-map #'expand-file-name)))

;;;; Git helpers:

;;;###autoload
(defun lab-git-current-branch ()
  "Return current branch's name."
  (s-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))

;;;###autoload
(defun lab-git-last-commit-sha ()
  "Return last commits SHA for current project."
  (s-trim (shell-command-to-string "git rev-parse HEAD")))

;;;###autoload
(defun lab-git-get-config (conf)
  "`git config --get CONF' wrapper."
  (s-trim
   (with-temp-buffer
     (call-process "git" nil t nil "config" "--get" conf)
     (buffer-string))))

;;;###autoload
(defun lab-git-remote-homepage ()
  "Generate API base homepage from repository origin remote URL."
  (let ((remote-url (lab-git-get-config "remote.origin.url")))
    (cond
     ((s-contains? "@" remote-url)
      (thread-last
        remote-url
        (s-split "@")
        (nth 1)
        (s-replace-regexp ":[0-9]\*/\*" "/")
        (s-chop-suffix ".git")
        (s-prepend "https://")))
     (t (s-chop-suffix ".git" remote-url)))))

;;;###autoload
(defun lab-git-dir? (dir)
  "Check if DIR is git version controlled directory."
  (file-directory-p (concat dir "/.git")))

;;;; Git helpers (interactive):

;;;###autoload
(defun lab-git-origin-switch-to-ssh ()
  "Switch remote origin address to SSH from HTTPS."
  (interactive)
  (if-let* ((https-origin (lab-git-get-config "remote.origin.url"))
            (it (s-match "https://\\(.*\\)\\.\\(com\\|net\\|org\\)/\\(.*\\)" https-origin))
            (ssh-origin (format "git@%s.%s:%s" (nth 1 it) (nth 2 it) (nth 3 it))))
      (progn
        (call-process "git" nil nil nil "remote" "set-url" "origin" ssh-origin)
        (message "Switched to SSH!"))
    (user-error "Already using SSH method or something is wrong with the current upstream address!")))

;;;###autoload
(cl-defun lab-git-clone (url dir &key shallow callback)
  "Clone URL to DIR.
DIR will be the parent directory of the repo you've just cloned.
You can think this as simple \"git clone URL\" call in DIR.

CALLBACK is called with either t or nil, indicating success
status.

Also see `lab-after-git-clone-functions'."
  (interactive
   (list
    (read-string "URL: ")
    (read-directory-name "Directory to clone in: " lab-projects-directory)))
  (make-directory dir t)
  (let* ((default-directory dir)
         (proc (start-process-shell-command
                "*lab-clone*" lab--git-clone-buffer-name
                (format "git clone --quiet %s \"%s\"" (if shallow "--depth=1" "") url))))
    (with-current-buffer lab--git-clone-buffer-name
      (goto-char (point-max))
      (insert (format ">> Cloning %s to %s...\n" url dir)))
    (set-process-sentinel
     proc
     (lambda (p _e)
       (if (= 0 (process-exit-status p))
           (let* ((repo-name (file-name-nondirectory (string-trim-right url "\\.git")))
                  (default-directory (concat (string-trim-right dir "/") "/" repo-name)))
             (when callback
               (funcall callback t)
               (with-current-buffer lab--git-clone-buffer-name
                 (goto-char (point-max))
                 (insert (format ">> Cloning %s to %s...Done\n" url dir))))
             (unless callback
               (message "%s is cloned." repo-name)
               (when (and (not (file-exists-p default-directory))
                          (not (eq lab-after-git-clone-functions nil)))
                 (error "Can't find the cloned repository at %s, unable to run hooks" default-directory))
               (seq-each #'funcall lab-after-git-clone-functions)))
         (if callback
             (progn
               (with-current-buffer lab--git-clone-buffer-name
                 (goto-char (point-max))
                 (insert (format ">> Cloning %s to %s...Failed!\n" url dir)))
               (funcall callback nil))
           (message "Cloning %s is failed." url)))))))

(async-defun lab-clone-bulk (gitlab-group root)
  "Clone all repositories of GITLAB-GROUP into ROOT."
  (interactive (list
      (read-string "Enter GitLab group path to clone all projects from: " lab-group)
      (read-directory-name "Path to clone projects in: " lab-projects-directory)))
  (let* ((projects (prog2 (message "Please wait, getting project list...")
                       (await (promise-new
                               (lambda (resolve reject)
                                 (lab-get-all-group-projects
                                  gitlab-group
                                  :on-success (lambda (data) (funcall resolve data))
                                  :on-error (lambda (data) (funcall reject data))))))
                     (message "Please wait, getting project list...Done")))
         (groupped (seq-group-by
                    (lambda (it)
                      (file-exists-p (lab--path-join root (alist-get 'path_with_namespace it))))
                    projects))
         ;; (existing (alist-get t groupped))
         (new (alist-get nil groupped)))
    (message "Found %s projects in total, %s are new." (length projects) (length new))
    (lab--clone-bulk-helper root new)))

(defun lab--clone-bulk-helper (root repositories)
  "Clone all REPOSITORIES to ROOT directory.
REPOSITORIES is a list containing repository alists in the form of

  \\='((path_with_namespace . \"group/subgroup/.../project_name\")
    (ssh_url_to_repo . \"ssh://...\")
    (https_url_to_repo . \"ssh://...\"))

This is typically returned by GitLab API.  Each project is cloned
under \"ROOT/path_with_namespace\", so it replicates the same
project hierarchy in GitLab in your local path ROOT.

If the inferred path for a project already exist in the
filesystem, that project will be simply skipped.

When called interactively, it asks for a path to clone projects
in (also see `lab-projects-directory') and also asks for a GitLab
group path to fetch all projects of (also see `lab-group').

You can interrupt the process by calling \\[lab-interrupt]."
  (if-let ((current (car repositories)))
      (let* ((path (lab--path-join root (alist-get 'path_with_namespace current)))
             (project-parent (f-dirname path)))
        (when lab--interrupt
          (setq lab--interrupt nil)
          (user-error "Pulling interrupted by user"))
        (if (file-exists-p path)
            (progn
              (message "lab :: Skipping %s as it already exists." path)
              (lab--clone-bulk-helper root (seq-drop repositories 1)))
          (mkdir project-parent t)
          (message "lab :: Cloning %s..." path)
          (lab-git-clone
           (alist-get (lab--clone-url-path-selector) current)
           project-parent
           :callback
           (lambda (success?)
             (message "lab :: Cloning %s...%s" path (if success? "Done" "Failed!"))
             (lab--clone-bulk-helper root (seq-drop repositories 1))))))
    (message "lab :: Cloned all repositories. Check buffer %s for details." lab--git-clone-buffer-name)))

(async-defun lab-pull-bulk (&optional repositories interactive?)
  "Pull all REPOSITORIES.
REPOSITORIES is a list of paths to different repositories.  When
called interactively, it asks you for a path (also see
`lab-projects-directory'), finds all git repositories under that
directory and pulls them using `lab-git-pull'.

See the following variables to control the behavior of pulling:
`lab-pull-bulk-switch-to-main', `lab-main-branch-name'.

You can interrupt the process by calling \\[lab-interrupt]."
  (interactive (list (lab-all-project-roots (read-directory-name "Path: " lab-projects-directory)) t))
  (with-current-buffer (get-buffer-create lab--pull-bulk-buffer)
    (condition-case reason
        (let ((failed '())
              (index 0))
          (erase-buffer)
          (when interactive?
            (switch-to-buffer-other-window (current-buffer)))
          (dolist (repository repositories)
            (insert (format ">> (%s/%s) Pulling %s...\n"
                            (1+ index)
                            (length repositories)
                            (abbreviate-file-name repository)))
            (when lab--interrupt
              (setq lab--interrupt nil)
              (user-error "Pulling interrupted by user"))
            (pcase-let ((`(,status ,output) (await (lab--pull-bulk-single repository))))
              (goto-char (point-max))
              (insert (format "%s\n" (string-trim (or output ""))))
              (insert
               (format
                ">> Exited with %s\n\n"
                (propertize (format "%s" status) 'face
                            `(:weight bold :foreground ,(pcase status
                                                          ('success "green")
                                                          (_ "red"))))))
              (unless (eq status 'success)
                (push repository failed)))
            (setq index (1+ index)))
          (if (seq-empty-p failed)
              (insert ">> Pulled all repositories successfully!")
            (insert "lab was unable to pull the following repositories:\n")
            (dolist (it failed)
              (goto-char (point-max))
              (insert (format "- %s\n" (abbreviate-file-name it))))))
      (error
       (let ((msg (format "Pulling %s with reason: %s"
                          (propertize "failed" 'face '(:foreground "red"))
                          reason)))
         (insert "\n" msg "\n")
         (message msg))))))

;;;; Core:

(defun lab--project-path (&optional safe?)
  "Return hexified project path for current git project.
This is mostly used while doing an api call for the current
project.

If SAFE? is non-nil, then check if the extracted project id really
matches with the `lab-host' and return nil if it does not.  Normally
non-safe checks are used because the host and the ssh address of the
host address may differ (see issue #5) but for some functions making
this check makes sense without any significant loss of functionality."
  (let ((remote-url (lab-git-get-config "remote.origin.url")))
    (when (or (not safe?) (and safe? (s-contains? (url-host (url-generic-parse-url lab-host)) remote-url)))
      (if (s-prefix? "http" remote-url)
          (thread-last
            remote-url
            (s-chop-suffix ".git")
            (s-chop-prefix lab-host)
            (s-chop-prefix "/")
            (s-chop-prefix "/")
            (s-chop-prefix "/")
            (s-trim)
            (url-hexify-string))
        (thread-last
          (s-split-up-to ":" remote-url 1)
          (cadr)
          (s-chop-suffix ".git")
          (s-trim)
          (url-hexify-string))))))

(defun lab--retrieve-token ()
  "Retrieve the GitLab API token to use."
  (or lab-token
      (when-let* ((entry (car (auth-source-search :host lab-host :max 1)))
                  (secret-fn (plist-get entry :secret)))
        (funcall secret-fn))))

;;;###autoload
(cl-defun lab--request
    (endpoint
     &rest params
     &key (%type "GET") %headers %params %data %collect-all? %raw? %success %error
     &allow-other-keys)
  "Make a sync or async GitLab API request to ENDPOINT with PARAMS.

Accepted keyword params:

- %TYPE        HTTP method (default \"GET\").
- %HEADERS     Additional HTTP headers (alist).
- %PARAMS      Additional URL query parameters (alist).
- %DATA        Data/body payload (plist or alist).
- %COLLECT-ALL If non-nil, auto-paginate and return all results as a list.
- %RAW?        If non-nil, return raw API response (string), otherwise parse
               as JSON/alist.
- %SUCCESS     If non-nil, call as callback with parsed data on success (async).
- %ERROR       If non-nil, call as callback with data on error.

Additional PARAMS are added to the endpoint as URL parameters.

Special endpoint interpolation:

  `#{project}` or `#{group}` will be replaced using buffer-local
  project/group context.

- If %COLLECT-ALL is given, fetches all pages and collates results.
- If %RAW? is given, skips JSON parsing and returns raw response.
- If %SUCCESS is given, makes request asynchronous and calls %SUCCESS
  with data.  %ERROR is the error callback.
- Otherwise, returns parsed response (synchronously).

Example:
  (lab--request
   \"projects/#{project}/pipelines\"
   :scope \"running\"
   :ref \"master\"
   :%collect-all t)"
  (setq params (lab--plist-remove-keys-with-prefix ":%" params))
  (let* ((token (lab--retrieve-token))
         (request-url
          (thread-last
            (if (s-prefix? "http" endpoint)
                endpoint
              (format "%s/api/v4/%s" lab-host endpoint))
            (s-replace-regexp "#{group}" (lambda (&rest _) (url-hexify-string lab-group)))
            (s-replace-regexp "#{project}" (lambda (&rest _)
                                             (or (ignore-errors (lab--project-path))
                                                 (user-error "You are not in a valid git project"))))))
         (make-request
          (lambda (lastid on-success)
            (request
              request-url
              :type %type
              :headers `((Authorization . ,(format "Bearer %s" token)) ,@%headers)
              :parser (if %raw?
                          #'buffer-string
                        (apply-partially
                         #'json-parse-buffer
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil))
              :success (cl-function (lambda (&key data &allow-other-keys) (funcall on-success data)))
              :error (cl-function
                      (lambda (&key data &allow-other-keys)
                        (if %error
                            (funcall %error data)
                          (error ">> lab--request failed with %s" data))))
              :sync (not %success) ;; Only sync if no user callback
              :data (if (plistp %data) (lab--plist-to-alist %data) %data)
              :params
              `(,@(when %collect-all?
                    `(("per_page" . ,lab--max-per-page-result-count) ("order_by" . "id") ("sort" . "asc") ("pagination" . "keyset")))
                ,@(when (and %collect-all? (not (eq lastid t)))
                    `(("id_after" . ,lastid)))
                ,@(unless %collect-all?
                    `(("per_page" . ,lab-result-count)))
                ,@(lab--plist-to-alist params) ,@%params)))))
    (cond
     ((and %success %collect-all?)
      (let (acc)
        (cl-labels
            ((step (lastid)
                   (funcall make-request lastid
                            (lambda (data)
                              (setq acc (append acc data))
                              (let ((len (length data))
                                    (newid (alist-get 'id (lab-last-item data))))
                                (if (and (= len lab--max-per-page-result-count) newid)
                                    (step newid)
                                  (funcall %success acc)))))))
          (step t))))
     ((and (not %success) %collect-all?)
      (let ((all-items '())
            (lastid t))
        (while lastid
          (funcall
           make-request lastid
           (lambda (data)
             (setq lastid (while (lab--length= data lab--max-per-page-result-count)
                            (alist-get 'id (lab-last-item data))))
             (setq all-items (append all-items data)))))
        all-items))
     (t
      (let ((json nil))
        (funcall make-request t
                 (lambda (data)
                   (setq json data)
                   (when %success
                     (funcall %success data))))
        json)))))

(defun lab--open-web-url (url)
  (kill-new url)
  (funcall lab-browse-url-fn url))

;;;; Projects:

(lab--define-actions-for project
  :formatter #'lab--format-project-title
  :keymap
  ((?o "Open"
       (lab--open-web-url .web_url))
   (?c "Clone"
       (lab-git-clone
        (alist-get (lab--clone-url-path-selector) it)
        (read-directory-name "Directory to clone in: " lab-projects-directory)))
   (?m "List merge requests"
       (lab-list-project-merge-requests .id))
   (?p "List pipelines"
       (lab-list-project-pipelines .id))
   (?t "Trigger pipeline"
       (lab-trigger-pipeline-manually .id .default_branch))
   (?i "Inspect"
       (lab--inspect-obj it))))

(cl-defun lab-get-all-group-projects (&optional group &key on-success on-error)
  "Return all groups belonging to GROUP.
If ON-SUCCESS is non-nil, call it asynchronously with the result;
ON-ERROR, if provided, handles errors."
  (lab--request
   (format "groups/%s/projects" (or (when group (url-hexify-string group)) "#{group}"))
   :with_shared 'false
   :include_subgroups 'true
   :%collect-all? t
   :%success on-success
   :%error on-error))

;;;###autoload
(defun lab-list-all-group-projects (&optional group)
  "List all GROUP projects and act on them.
See `lab-group'.  May be slow if there are many projects."
  (interactive)
  (lab-get-all-group-projects
   group
   :on-success
   (lambda (data)
     (message ">> Fetching projects, this may take a while...")
     (lab-project-select-and-act-on data))))

(cl-defun lab-get-all-owned-projects (&key on-success on-error)
  "Get all projects owned by you.
If ON-SUCCESS is non-nil, call it asynchronously with the result;
ON-ERROR, if provided, handles errors."
  (lab--request
   "projects"
   :owned 'true
   :%collect-all? t
   :%success on-success
   :%error on-error))

;;;###autoload
(defun lab-list-all-owned-projects ()
  "Get all projects owned by the you and act on them.
May be slow if there are many projects."
  (interactive)
  (lab-get-all-owned-projects
   :on-success
   (lambda (data)
     (message ">> Fetching projects, this may take a while...")
     (lab-project-select-and-act-on data))))

;;;###autoload
(defun lab-list-project-merge-requests (&optional project)
  "List all open MRs that belongs to PROJECT.
If it's omitted, currently open project is used."
  (interactive (list (lab--read-project-id-interactive-helper)))
  (lab--within-current-project
   (lab-merge-request-select-and-act-on
    (lab--request
     (format "projects/%s/merge_requests" (or project "#{project}"))
     :scope 'all))))

;;;###autoload
(defun lab-get-project-pipelines (&optional project)
  "Get pipelines for PROJECT.
If PROJECT is nil, current git project is used."
  (lab--request
   (format "projects/%s/pipelines" (or project "#{project}"))))

;;;###autoload
(defun lab-list-project-pipelines (&optional project)
  "List latest pipelines belonging to PROJECT.
If PROJECT is nil,current git project is used."
  (interactive (list (lab--read-project-id-interactive-helper)))
  (lab--within-current-project
   (lab-pipeline-select-and-act-on
    (lab--sort-by-latest-updated
     (lab-get-project-pipelines project)))))

;;;###autoload
(defun lab-act-on-last-project-pipeline (&optional project)
  "List latest pipelines belonging to PROJECT.
If PROJECT is nil,current git project is used."
  (interactive (list (lab--read-project-id-interactive-helper)))
  (lab--within-current-project
   (lab-pipeline-act-on
    (car (let ((lab-result-count 1))
           (lab-get-project-pipelines project))))))

;;;###autoload
(defun lab-search-project ()
  "Search in all GitLab projects."
  (interactive)
  ;; Need to generalize this pattern if I start using consult more in
  ;; this package
  (lab-project-act-on
   (if (lab--use-consult?)
       (consult--read
        (lab--consult-async-generator
         (lambda (action on-result)
           (lab--request "projects" :search action :%success on-result))
         (lambda (result)
           (mapcar
            (lambda (cand)
              (lab--with-text-properties (lab--format-project-title cand) :item cand))
            result)))
        :lookup
        (lambda (selected candidates &rest _)
          (lab--get-text-property (car (member selected candidates)) :item))
        :prompt "Select project: "
        :category 'lab-project
        :sort nil
        :require-match t
        :async-wrap #'lab--consult-async-wrapper)
     (let ((input (read-string "Search project: ")))
       (lab--completing-read-object
        "Search project: "
        (lab--request "projects" :search input)
        :formatter #'lab--format-project-title
        :category 'lab-project
        :sort? nil)))))

;;;; Pipelines:

(lab--define-actions-for pipeline
  :formatter #'lab--format-pipeline
  :keymap
  ((?o "Open"
       (lab--open-web-url .web_url))
   (?r "Retry"
       (lab--request
        (format "projects/%s/pipelines/%s/retry" .project_id .id)
        :%type "POST")
       (when lab-pipeline-automatically-watch-after-retry
         (lab-watch-pipeline .web_url)))
   (?c "Cancel"
       (lab--request
        (format "projects/%s/pipelines/%s/cancel" .project_id .id)
        :%type "POST"))
   (?d "Delete"
       (lab--request
        (format "projects/%s/pipelines/%s" .project_id .id)
        :%type "DELETE"))
   (?w "Watch"
       (lab-watch-pipeline .web_url))
   (?j "List jobs"
       (lab-list-pipeline-jobs .project_id .id))
   (?i "Inspect"
       (lab--inspect-obj it))
   (?I "Inspect detailed"
       (lab--inspect-obj
        (lab-get-pipeline .project_id .id)))))

;;;###autoload
(defun lab-get-pipeline (project-id pipeline-id)
  "Get detailed information for PIPELINE-ID in PROJECT-ID."
  (lab--request (format "projects/%s/pipelines/%s" project-id pipeline-id)))

;;;###autoload
(defun lab-get-pipeline-jobs (project-id pipeline-id)
  "Get latest jobs for PIPELINE-ID in PROJECT-ID."
  (lab--request
   (format "projects/%s/pipelines/%s/jobs" project-id pipeline-id)))

;;;###autoload
(defun lab-list-pipeline-jobs (project-id pipeline-id)
  "List latest jobs for PIPELINE-ID in PROJECT-ID."
  (lab-job-select-and-act-on
   (lab-get-pipeline-jobs project-id pipeline-id)))

;;;###autoload
(defun lab-watch-pipeline (url &optional rerun?)
  "Start watching pipeline URL status.
Send a notification if it's finished, failed or waiting for a
manual action.  RERUN? is used to indicate that this is a
recurring call, instead of a new watch request."
  (interactive "sPipeline URL: ")
  (let* ((data (s-match (rx (literal lab-host) (regexp "/\\(.*\\)/-/pipelines/\\([0-9]+\\)")) url))
         (project (nth 1 data))
         (project-hexified (url-hexify-string project))
         (pipeline (nth 2 data)))
    (when (or (not pipeline) (s-blank? pipeline))
      (user-error "Pipeline id is nil for %s" url))
    (unless rerun?
      (message ">> Started watching pipeline %s on %s!" pipeline project))
    (run-with-timer
     (if rerun? lab-pipeline-watcher-debounce-time 1)
     nil
     (lambda ()
       (let-alist (lab--request (format "projects/%s/pipelines/%s" project-hexified pipeline))
         (pcase .status
           ("success"
            (lab--alert (format "Pipeline FINISHED: %s/%s" project pipeline)))
           ("failed"
            (lab--alert (format "Pipeline FAILED: %s/%s" project pipeline)))
           ((or "canceled" "skipped" "scheduled")
            (lab--alert (format "Pipeline %s: %s/%s" (s-upcase .status) project pipeline)))
           ("manual"
            (lab--alert (format "Pipeline requires MANUAL action: %s/%s" project pipeline))
            (when lab-should-open-pipeline-on-manual-action?
              (browse-url .web_url)))
           (_
            (lab-watch-pipeline url t))))))))

;;;###autoload
(defun lab-watch-pipeline-for-last-commit ()
  "Start watching the pipeline created by your last commit."
  (interactive)
  (unless (s-prefix? lab-host (lab-git-remote-homepage))
    (user-error "Not a valid Gitlab repo"))
  (let ((project (lab--project-path))
        (sha (lab-git-last-commit-sha)))
    ;; Pipeline may take some time to appear
    (run-with-timer
     lab-pipeline-watcher-initial-delay nil
     (lambda ()
       (if-let* ((lab-result-count 3)
                 (pipeline (seq-find
                            (lambda (it) (equal sha (alist-get 'sha it)))
                            (lab-get-project-pipelines project))))
           (lab-watch-pipeline (alist-get 'web_url pipeline))
         (user-error "Seems like there are no pipelines created for your last commit"))))))

;;;###autoload
(defun lab-watch-merge-request-last-pipeline (mr)
  "Start watching the latest pipeline of given MR."
  ;; Pipeline may take some time to appear
  (run-with-timer
   lab-pipeline-watcher-initial-delay nil
   (lambda ()
     (let* ((lab-result-count 1)
            (pipeline (car (lab-get-merge-request-pipelines
                            (alist-get 'project_id mr)
                            (alist-get 'iid mr)))))
       (lab-watch-pipeline (alist-get 'web_url pipeline))))))

(defun lab-trigger-pipeline-manually (&optional project-id default-branch)
  "Trigger a pipeline.
This prompts for the branch or tag to start the pipeline and lets you
enter additional environment variables interactively."
  (interactive)
  ;; TODO: Maybe list branches/tags & let the user select instead.
  (let ((ref (read-string
              "Branch or tag name: "
              (or default-branch (lab--find-main-branch)))))
    (lab--user-input :mode #'sh-mode
                     :init lab--trigger-pipeline-user-input-helper-text
                     :history-key (format "pipeline-variables-for-%s" (or project-id (lab--project-path)))
                     :parser
                     (lambda ()
                       (thread-last
                         (string-split (buffer-substring-no-properties (point-min) (point-max)) "\n")
                         (seq-map #'string-trim)
                         (seq-filter (lambda (it) (not (string-prefix-p "#" it))))
                         (seq-map (lambda (it)
                                    (pcase-let ((`(,key ,val) (s-split-up-to "=" it 2)))
                                      (when (and key val)
                                        (cons key val)))))
                         (seq-filter #'identity)))
                     :on-accept
                     (lambda (_ variables)
                       (lab-trigger-pipeline :project-id project-id :ref ref :variables variables)))))

(cl-defun lab-trigger-pipeline (&key project-id ref variables)
  "Trigger a new pipeline for PROJECT-ID using the branch/tag name REF.

  Same thing as `lab-trigger-pipeline-manually' but for programmatic use
  only.

  If PROJECT-ID is nil, then use current project.

  VARIABLES is an alist, like:

  \\='((\"SOME_VAR\" . \"true\"))"
  (lab--request (format "projects/%s/pipeline" (or project-id "#{project}"))
                :ref ref
                :%type "POST"
                :%data (json-encode `((variables
                                       .
                                       ,(seq-map (pcase-lambda (`(,key . ,val))
                                                   `((key . ,key)
                                                     (value . ,(format "%s" val))))
                                                 variables))))))

;;;; lab-trace-mode:

;; TODO Add retry action for `lab-trace-mode-current-job' and start watching it
;; automatically

(defvar-local lab-trace-mode-current-job nil)

(defun lab-trace-mode-open-externally ()
  "Open current log in external browser."
  (interactive nil lab-trace-mode)
  (lab--open-web-url (alist-get 'web_url lab-trace-mode-current-job)))

(defvar-local lab-trace-mode-current-job nil
  "Currently inspected job object.")

(defvar lab-trace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c &") #'lab-trace-mode-open-externally)
    map)
  "Keymap for `lab-trace-mode'.")

(define-derived-mode lab-trace-mode prog-mode "lab-trace-mode"
  "Major mode for showing trace of a job.")

(defun lab-show-job-logs (job)
  "Show logs for JOB in a seperate buffer."
  (let-alist job
    (with-current-buffer (get-buffer-create (format "*lab-trace:%s-%s*" .id .name))
      (lab-trace-mode)
      (setq-local lab-trace-mode-current-job job)
      (insert
       (ansi-color-apply
        (replace-regexp-in-string
         "\r" "\n"
         (lab--request
          (format "projects/%s/jobs/%s/trace"
                  (lab--projid-for-job job)
                  .id)
          :%raw? t))))
      (switch-to-buffer-other-window (current-buffer)))))

;;;; Jobs:

(lab--define-actions-for job
  :formatter #'lab--format-job
  :keymap
  ((?o "Open"
       (lab--open-web-url .web_url))
   (?r "Retry"
       (lab--request
        (format "projects/%s/jobs/%s/retry" (lab--projid-for-job it) .id)
        :%type "POST"))
   (?c "Cancel"
       (lab--request
        (format "projects/%s/jobs/%s/cancel" (lab--projid-for-job it) .id)
        :%type "POST"))
   (?d "Delete"
       (lab--request
        (format "projects/%s/jobs/%s/erase" (lab--projid-for-job it) .id)
        :%type "POST"))
   (?t "Trace/logs"
       (lab-show-job-logs it))
   (?i "Inspect"
       (lab--inspect-obj it))
   (?I "Inspect detailed"
       (lab--inspect-obj
        (lab-get-job (lab--projid-for-job it) .id)))))

(defun lab-get-job (project-id job-id)
  "Get detailed information for JOB-ID in PROJECT-ID."
  (lab--request (format "projects/%s/jobs/%s" project-id job-id)))

;;;###autoload
(defun lab-act-on-last-failed-pipeline-job (&optional project-id)
  "List and act on last failed pipelines jobs for PROJECT.
  If PROJECT-ID is omitted, currently open project is used."
  (interactive (list (lab--read-project-id-interactive-helper)))
  (let* ((failed?
          (lambda (it)
            (equal (downcase (alist-get 'status it)) "failed")))
         (last-failed-pipeline (seq-find failed? (lab-get-project-pipelines (or project-id "#{project}")))))
    (if last-failed-pipeline
        (let-alist last-failed-pipeline
          (let ((jobs (seq-filter failed? (lab-get-pipeline-jobs .project_id .id))))
            (if (and jobs (> (length jobs) 0))
                (lab-job-select-and-act-on jobs)
              (user-error "A failed pipeline found but no failed job is found, see %s" .web_url))))
      (user-error "Not a single failed pipeline, congrats :)"))))

;;;; Merge Requests:

(lab--define-actions-for merge-request
  :formatter #'lab--format-merge-request-title
  :keymap
  ((?o "Open"
       (lab--open-web-url .web_url))
   (?c "Copy url"
       (kill-new .web_url))
   (?m "Mark as ready"
       (let ((result
              (lab--request
               (format "projects/%s/merge_requests/%s" .project_id .iid)
               :%type "PUT"
               :%data (list :title (s-chop-prefixes '("WIP: " "Draft: ") .title)))))
         (seq-each
          (lambda (it) (funcall it result))
          lab-after-merge-request-mark-ready-functions)))
   (?r "Rebase"
       ;; TODO add a function that rebases merge request with given
       ;; URL and write this in terms of the new function
       (lab--request
        (format "projects/%s/merge_requests/%s/rebase" .project_id .iid)
        :%type "PUT"))
   (?p "Pipelines"
       (lab-pipeline-select-and-act-on
        (lab--sort-by-latest-updated
         (lab--request
          (format "projects/%s/merge_requests/%s/pipelines" .project_id .iid)
          :scope 'all
          :state 'opened))))
   (?w "Watch last pipeline"
       (lab-watch-merge-request-last-pipeline it))
   (?i "Inspect"
       (lab--inspect-obj it))))

(defun lab-act-on-merge-request (url)
  "Act on given merge request URL."
  (interactive "sMerge Request URL: ")
  (lab-merge-request-act-on
   ;; No need to do a request for full MR object because every field
   ;; we need for actions are available on the URL.
   (lab--parse-merge-request-url url)))

(defun lab-get-merge-request-pipelines (project-id mr-id)
  "Get last pipelines for MR-ID in PROJECT-ID."
  (lab--request
   (format "projects/%s/merge_requests/%s/pipelines" project-id mr-id)))

(defun lab-list-branch-merge-requests ()
  "List all open MRs that the source branch is the current branch."
  (interactive)
  (lab-merge-request-select-and-act-on
   (lab--sort-by-latest-updated
    (lab--request
     "projects/#{project}/merge_requests"
     :scope 'all
     :state 'opened
     :source_branch (lab-git-current-branch)))))

(defun lab-list-my-merge-requests ()
  "List all of your currently open merge requests.
`Your' means either it's created by you or assigned to you."
  (interactive)
  (lab-merge-request-select-and-act-on
   (lab--sort-by-latest-updated
    `(,@(lab--request
         "merge_requests"
         :scope 'created_by_me
         :state 'opened)
      ,@(lab--request
         "merge_requests"
         :scope 'assigned_to_me
         :state 'opened)))))

(defun lab-list-group-merge-requests (&optional group)
  "List all open MRs that belongs to GROUP.
If GROUP is omitted, `lab-group' is used."
  (interactive)
  (lab-merge-request-select-and-act-on
   (lab--sort-by-latest-updated
    (lab--request
     (format "groups/%s/merge_requests" (or (when group (url-hexify-string group)) "#{group}"))
     :scope 'all
     :state 'opened))))

(defun lab--remove-diff-buffer ()
  (when-let (buffer (get-buffer-window lab--diff-buffer-name))
    (quit-window t buffer)))

(defun lab--find-main-branch ()
  "Find the main branch for current repository.
Main branch is one the branch names listed in `lab-main-branch-name'."
  (let ((branches (vc-git-branches)))
    (seq-find
     (lambda (it) (member it branches))
     (lab--listify lab-main-branch-name))))

(declare-function markdown-mode "markdown-mode")
(defun lab-create-merge-request ()
  "Create an MR interactively for current git project."
  (interactive)
  (let* ((currbuf (current-buffer))
         (branches (vc-git-branches))
         (source-branch (completing-read
                         "Source branch: "
                         branches
                         nil nil (lab-git-current-branch)))
         (target-branch (completing-read
                         "Target branch: "
                         branches
                         nil nil
                         (lab--find-main-branch)))
         (title (read-string "MR Title: "
                             (s-trim (shell-command-to-string "git log -1 --pretty=%B")))))
    (lab--user-input
     :mode (if (require 'markdown-mode nil t) #'markdown-mode #'prog-mode)
     :init (thread-last
             (append
              lab-default-merge-request-create-options
              (list :source_branch source-branch
                    :target_branch target-branch
                    :title title))
             (map-apply
              (lambda (key val)
                (format "%s: %s" (substring (symbol-name key) 1) (lab--serialize-yaml-value val))))
             (nreverse)
             (s-join "\n")
             (s-prepend "---\n")
             (s-append "\n---\n\n"))
     :parser #'lab--parse-merge-request-buffer
     :on-start
     (lambda ()
       (vc-diff-internal t (vc-deduce-fileset t) target-branch source-branch t lab--diff-buffer-name))
     :on-reject #'lab--remove-diff-buffer
     :on-accept
     (lambda (_ data)
       (with-current-buffer currbuf
         (let ((result
                (lab--request
                 "projects/#{project}/merge_requests"
                 :%type "POST"
                 :%data data)))
           (lab--remove-diff-buffer)
           (seq-each
            (lambda (it) (funcall it result))
            lab-after-merge-requests-create-functions)
           (lab--open-web-url (alist-get 'web_url result))))))))

(defun lab--parse-merge-request-buffer ()
  (goto-char (point-min))
  (let* ((yaml (when (search-forward-regexp lab--regex-yaml-metadata-border nil t)
                 (buffer-substring-no-properties
                  (point)
                  (search-forward-regexp lab--regex-yaml-metadata-border nil t))))
         (yaml-data (when yaml
                      (thread-last
                        yaml
                        (s-split "\n")
                        (mapcar (lambda (it) (mapcar #'s-trim (s-split-up-to ": " it 1))))
                        (seq-filter (lambda (it)
                                      (and (lab--length= it 2)
                                           (not (s-blank? (car it))))))
                        (mapcar (lambda (it)
                                  (list (intern (concat ":" (car it)))
                                        (lab--deserialize-yaml-value (cadr it)))))
                        (apply #'seq-concatenate 'list)))))
    (map-insert yaml-data
                :description (s-trim (buffer-substring-no-properties (point) (point-max))))))

(defun lab--parse-merge-request-url (url)
  "Parse given merge request URL into a merge request object.

>> (let ((lab-host \"https://mycompany.gitlab.com\"))
     (lab--parse-merge-request-url \"https://mycompany.gitlab.com/some/project/path/-/merge_requests/579\"))
=> ((web_url . \"https://mycompany.gitlab.com/some/project/path/-/merge_requests/579\")
    (project_id . \"some%2Fproject%2Fpath\")
    (iid . \"579\"))

>> (let ((lab-host \"https://mycompany.gitlab.com/\"))
     (lab--parse-merge-request-url \"https://mycompany.gitlab.com/some/project/path/-/merge_requests/579/\"))
=> ((web_url . \"https://mycompany.gitlab.com/some/project/path/-/merge_requests/579/\")
    (project_id . \"some%2Fproject%2Fpath\")
    (iid . \"579\"))"
  (pcase-let* ((`(,project_id ,iid)
                (->>
                 (s-chop-prefix (concat (s-chop-suffix "/" lab-host) "/") url)
                 (s-split "/-/merge_requests/"))))
    `((web_url . ,url)
      (project_id . ,(url-hexify-string project_id))
      (iid . ,(car (string-split iid "[/#]"))))))

;;;; TODOs:

(lab--define-actions-for todo
  :formatter #'lab--format-todo
  :keymap
  ((?o "Open"
       (lab--open-web-url .target_url))
   (?d "Mark as done" (lab--request
                       (format "todos/%s/mark_as_done" .id)
                       :%type "POST"))
   (?i "Inspect"
       (lab--inspect-obj it))))

(defun lab-list-todos ()
  "List all todos for current user."
  (interactive)
  (lab-todo-select-and-act-on
   (lab--request "todos")))

(defun lab-mark-done-all-todos ()
  "Mark all todos as done."
  (interactive)
  (lab--request
   "todos/mark_as_done"
   :%type "POST"))

;;;; Org-mode integration

(declare-function org-table-align "org")

(defun org-dblock-write:lab-merge-requests (params)
  "Fetch and draw a table of merge requests based on PARAMS.
PARAMS is an plist where the keys are:

- :HEADERS is a list of headers for the table, each item is a
  JSON path like string for fetching value from the GitLab
  response.  The default is \\='(\"state\" \"title\"
  \"author.username\").

- :TYPE is the type of the request, possible values are `mine',
  `group', `project'.  `mine' fetches the merge requests that is
  either created by you or assigned to you.  `group' fetches the
  merge requests that are opened under your GitLab group.  By
  default `lab-group' is used as your group, or you can also
  supply :GROUP parameter to override it.  `project' fetches
  merge requests that belongs to current project or you can use
  :PROJECT to override it.  It should be either GitLab project id
  or project path like \"a-group-name/project\".

- :STATE is the state of the merge requests. Possible states are
  `opened', `closed', `merged' or `all'.  Default is `opened'.

- :SCOPE is the scope of the merge requests.  Please see GitLab
  API documentation for more information.  The default is `all'.

- :LIMIT is the maximum count of results.  By default it's
  `lab-result-count'.  The maximum is
  `lab--max-per-page-result-count'.

- For :GROUP and :PROJECT, see the explanation for :TYPE."
  (let* ((headers (or (plist-get params :headers) '("state" "title" "author.username")))
         ;; Possible types: mine, group, project
         (type (or (plist-get params :type) 'mine))
         ;; Possible states :: opened, closed, locked, merged, all
         (state (or (plist-get params :state) 'opened))
         (group (url-hexify-string (or (plist-get params :group) lab-group)))
         (scope (or (plist-get params :scope) 'all))
         ;; TODO (or ... current-project?)
         (project (or (plist-get params :project)))
         (lab-result-count (or (plist-get params :limit) lab-result-count))
         (mrs
          (pcase type
            ('mine (lab--sort-by-latest-updated
                    `(,@(lab--request
                         "merge_requests"
                         :scope 'created_by_me
                         :state state)
                      ,@(lab--request
                         "merge_requests"
                         :scope 'assigned_to_me
                         :state state))))
            ('group (lab--sort-by-latest-updated
                     (lab--request
                      (format "groups/%s/merge_requests" group)
                      :scope scope
                      :state state)))
            ;; TODO: Maybe also add branch? :source_branch (or branch (lab-git-current-branch))
            ('project (lab--request
                       (format "projects/%s/merge_requests" (or project "#{project}"))
                       :scope scope
                       :state state)))))
    (insert "|")
    (seq-do
     (lambda (it ) (insert (s-titleize (s-replace-all '(("_" . " ") ("." . " ")) it)) "|"))
     headers)
    (insert "\n|--")
    (seq-do
     (lambda (mr)
       (insert "\n|")
       (seq-do
        (lambda (header)
          (insert
           (let ((val (s-replace "|" "ǀ" (lab--alist-path-get (mapcar (lambda (it) (intern it)) (s-split "\\." header)) mr))))
             (format "%s"
                     (if (equal header "title")
                         (format "[[%s][%s]]" (alist-get 'web_url mr) val)
                       val)))
           "|"))
        headers))
     mrs)
    (org-table-align)))

;;;; Formatters & other helpers:

(defun lab--format-merge-request-title (mr)
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

(defun lab--format-project-title (project)
  (alist-get 'name_with_namespace project))

(defun lab--format-todo (todo)
  (let* ((time (lab--time-ago (date-to-time (alist-get 'created_at todo))))
         (person (alist-get 'username (alist-get 'author todo)))
         (type (alist-get 'target_type todo))
         (project-name (alist-get 'name (alist-get 'project todo)))
         (group-name (alist-get 'name (alist-get 'group todo)))
         (title (alist-get 'title (alist-get 'target todo)))
         (name (pcase type
                 ("Issue" (format "%s#%s" project-name title))
                 ("MergeRequest" (format "%s!%s" project-name title))
                 ("Epic" (format "%s/%s" group-name title))
                 (_ title))))
    (format "%-15s: %s@%s:%s %s"
            time
            (propertize person 'face '(:weight thin :slant italic))
            (alist-get 'target_type todo)
            (propertize name 'face 'bold)
            (alist-get 'body todo))))

(defun lab--fontify-status (status)
  (propertize
   (upcase status) 'face
   `(:foreground ,(pcase (downcase status)
                    ("created" "blue")
                    ("running" "yellow")
                    ("failed" "red")
                    ("success" "green")
                    ("manual" "orange")))))

(defun lab--format-pipeline (it)
  (let-alist it
    (format
     "%8s | %8s, %6s → %s (%s)%s"
     (lab--fontify-status .status)
     .id .source .ref
     (lab--time-ago (date-to-time .updated_at))
     (if .user (format " by %s" (alist-get 'username .user)) ""))))

(defun lab--format-job (it)
  (let-alist it
    (format
     "%8s | %8s, %15s on %s%s"
     (lab--fontify-status .status) .id (propertize .name 'face 'bold) .ref
     (if .user
         (format " by %s"  (propertize (alist-get 'username .user) 'face 'italic))
       ""))))

(defun lab--clone-url-path-selector ()
  (pcase lab-clone-method
    ('ssh 'ssh_url_to_repo)
    ('https 'https_url_to_repo)))

(defun lab--sort-by-latest-updated (lst)
  (seq-sort
   (lambda (o1 o2) (string> (alist-get 'updated_at o1) (alist-get 'updated_at o2)))
   lst))

(defun lab--serialize-yaml-value (val)
  (pcase val
    ((or 't "t" "true" "yes") "true")
    ((or "false" "no" :json-false 'nil) "false")
    (_ val)))

(defun lab--deserialize-yaml-value (val)
  (pcase val
    ((or 't "t" "true" "yes") t)
    ((or "false" "nil" "no" ":json-false") "false")
    (_ val)))

(defun lab--time-ago (past)
  (let* ((intervals '((31536000 . "year")
                      (2592000 . "month")
                      (86400 . "day")
                      (3600 . "hour")
                      (60 . "minute")
                      (1 . "second")
                      (0 . "now")))
         (secs (floor (-
                       (string-to-number (format-time-string "%s"))
                       (string-to-number (format-time-string "%s" past)))))
         (int (seq-find (lambda (it) (< (car it) secs)) intervals))
         (count (floor (/ secs (car int)))))
    (format
     "%s %s%s ago"
     count (cdr int) (if (> count 1) "s" ""))))

(defun lab--projid-for-job (job)
  (or (alist-get 'project_id job) (alist-get 'project_id (alist-get 'pipeline job))))

(declare-function alert "alert")
(defun lab--alert (msg)
  (message ">> lab.el :: %s" msg)
  (when (require 'alert nil t)
    (alert msg :title "lab.el")))

(provide 'lab)

;;; lab.el ends here
