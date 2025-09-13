;;; jsts.el --- JS/TS tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Diogo Doreto
;;
;; Author: Diogo Doreto <diogo@doreto.com.br>
;; Version: 0.0.1
;; Keywords: convenience tools javascript typescript
;; URL: https://github.com/DiogoDoreto/emacs-jsts
;; Package-Requires: ((emacs "24.3") (transient "0.9.3"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;  Collection of tools to work with JavaScript/TypeScript projects.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'transient)

;;; * Declarations
;;
;; A bunch of variable and function declarations
;; needed to appease the byte-compiler.
(declare-function tramp-archive-file-name-p "tramp-archive")

;;; * Customization
(defgroup jsts nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/DiogoDoreto/emacs-jsts"))

(defcustom jsts-lockfile-to-manager-alist
  '(("package-lock.json" . npm)
    ("bun.lock"          . bun)
    ("bun.lockb"         . bun)
    ("deno.lock"         . deno)
    ("yarn.lock"         . yarn)
    ("pnpm-lock.yaml"    . pnpm))
  "Alist mapping lockfile file names to their respective package manager."
  :type  '(alist :key-type string :value-type symbol)
  :group 'jsts)

(defcustom jsts-licenses-file
  (expand-file-name "jsts-licenses.eld"
                    user-emacs-directory)
  "Name and location of the JSTS's known licenses file."
  :type 'string
  :group 'jsts)

;;; * Project root
;;
;; Heavily inspired by how Projectile manages its projects
(defvar jsts-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `jsts-project-root'.")

(defun jsts--clear-project-cache-for-dir (dir)
  "Clear project cache for directory DIR and all its children."
  (maphash (lambda (cached-dir _root)
             (when (string-prefix-p dir cached-dir)
               (remhash cached-dir jsts-project-root-cache)))
           jsts-project-root-cache))

(defun jsts-project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  ;; Adapted from `projectile-project-root' function
  (let ((dir (or dir default-directory)))
    ;; Back out of any archives, the project will live on the outside and
    ;; searching them is slow.
    (when (and (fboundp 'tramp-archive-file-name-archive)
               (tramp-archive-file-name-p dir))
      (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
    ;; the cached value will be 'none in the case of no project root (this is to
    ;; ensure it is not reevaluated each time when not inside a project) so use
    ;; cl-subst to replace this 'none value with nil so a nil value is used
    ;; instead
    (cl-subst nil 'none
              (or
               ;; use the cached value, if it exists
               (gethash dir jsts-project-root-cache)
               ;; if the file isn't local, and we're not connected, don't try to
               ;; find a root now now, but don't cache failure, as we might
               ;; re-connect.  The `is-local' and `is-connected' variables are
               ;; used to fix the behavior where Emacs hangs because of Jsts
               ;; when you open a file over TRAMP. It basically prevents Jsts
               ;; from trying to find information about files for which it's not
               ;; possible to get that information right now.
               (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
                     (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                 (unless (or is-local is-connected)
                   'none))
               ;; if the file is local or we're connected to it via TRAMP, run
               ;; through the known lockfiles until we find a project dir
               (cl-some
                (lambda (pair)
                  (let* ((lockfile (car pair))
                         (rootdir (locate-dominating-file (file-truename dir) lockfile)))
                    (when rootdir
                      (puthash dir rootdir jsts-project-root-cache)
                      rootdir)))
                jsts-lockfile-to-manager-alist)
               ;; if no lockfile was found, try looking for a ~package.json~ as
               ;; it may not have been installed yet. We first find the closest
               ;; file and then try finding it again in case this is part of a
               ;; monorepo.
               (let* ((closest (locate-dominating-file (file-truename dir) "package.json"))
                      (parent (and closest
                                   (file-name-directory (directory-file-name closest))))
                      (furthest (and parent
                                     (locate-dominating-file parent "package.json")))
                      (rootdir (or furthest closest)))
                 (when rootdir
                   (puthash dir rootdir jsts-project-root-cache)
                   rootdir))
               ;; if we get here, we have failed to find a root by all
               ;; conventional means, and we assume the failure isn't transient
               ;; / network related, so cache the failure
               (progn
                 (puthash dir 'none jsts-project-root-cache)
                 'none)))))

(defun jsts-ensure-project (&optional dir)
  "Ensures we are operating in a JS/TS project."
  (or (jsts-project-root dir)
      (user-error "Not a JS/TS project.")))

;;; * License helpers

(defun jsts-update-licenses-cache ()
  "Download license list from SPDX and update the contents of the
`jsts-licenses-file'"
  (interactive)
  (url-retrieve
   "https://raw.githubusercontent.com/spdx/license-list-data/refs/heads/main/json/licenses.json"
   (lambda (_)
     (goto-char (point-min))
     (search-forward "\n\n") ;; Skip HTTP headers
     (let* ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (licenses (alist-get 'licenses (json-read)))
            (osi-licenses (seq-filter (apply-partially #'alist-get 'isOsiApproved) licenses))
            (license-ids (mapcar (apply-partially #'alist-get 'licenseId) osi-licenses)))
       (with-temp-file jsts-licenses-file
         (insert (let (print-length) (prin1-to-string license-ids))))
       (message "License list updated.")))))

(defun jsts-osi-licenses ()
  "List of OSI approved license IDs"
  (with-demoted-errors "Error while reading licenses file: %S"
    (when (file-exists-p jsts-licenses-file)
      (with-temp-buffer
        (insert-file-contents jsts-licenses-file)
        (read (current-buffer))))))

;;; * package.json

(defun jsts--package-json-parse (file)
  (let ((json-object-type 'alist)
        (json-key-type 'string)
        (json-array-type 'list))
    (ignore-errors (json-read-file file))))

(defun jsts--package-json-get-name (parsed-file)
  (alist-get "name" parsed-file nil nil #'string=))

(defun jsts--package-json-get-scripts (parsed-file)
  (alist-get "scripts" parsed-file nil nil #'string=))

;;; * View package

(defun jsts--view-package-data (package-name)
  "Run the info command and return the parsed the output"
  (let ((pm (or (jsts-package-manager) 'npm)))
    (when (eq pm 'deno)
      ;; Deno's info structure is too different
      (user-error "Deno is not yet supported for this action."))
    (let ((response (shell-command-to-string
                     (format "%s info --json %s" pm package-name))))
      (unless (string-prefix-p "{" response)
        (user-error "Error: %s" response))
      (json-read-from-string response))))

(defun jsts--make-url-button (url)
  (with-temp-buffer
    (insert-text-button
     url
     'action (lambda (_) (browse-url url))
     'help-echo (format "Go to %s" url)
     'follow-link t)
    (buffer-string)))

(defun jsts--insert-list (rows)
  "Insert ROWS as a list. Each row is a list of column strings. Columns are
padded to the maximum width in their respective column."
  (when rows
    (let* ((num-cols (length (car rows)))
           (col-widths
            (cl-loop for i from 0 below num-cols
                     collect (apply #'max (mapcar (lambda (row)
                                                    (length (nth i row)))
                                                  rows)))))
      (dolist (row rows)
        (insert "-")
        (dotimes (i num-cols)
          (insert ?\s (string-pad (nth i row) (nth i col-widths))))
        (insert "\n")))))

(defun jsts--insert-dependency-section (title dependencies)
  "Insert a collapsible list of dependencies with TITLE and CONTENT at point."
  (insert (format "\n%s (%s): " title (length dependencies)))
  (let (ov ov-start)
    (insert-text-button "[toggle]"
                        'action (lambda (_)
                                  (overlay-put ov 'invisible
                                               (not (overlay-get ov 'invisible)))))
    (insert "\n")
    (setq ov-start (point))
    (jsts--insert-list
     (mapcar (lambda (dep)
               (list (propertize (symbol-name (car dep))
                                 'face 'font-lock-keyword-face)
                     (cdr dep)))
             dependencies))
    (setq ov (make-overlay ov-start (point)))
    (overlay-put ov 'invisible t)))

(defun jsts-view-package (package-name)
  "Display information about PACKAGE-NAME in a help window, using project's
package manager (or npm when none is identified) to fetch the data."
  (interactive "sPackage name: ")
  (let* ((data (jsts--view-package-data package-name))
         (buf-name (format "*jsts-view-package: %s*" package-name)))
    (save-excursion
      (with-help-window (get-buffer-create buf-name)
        (with-current-buffer standard-output
          (insert (format "%s@%s | %s\n%s\n"
                          (propertize (alist-get 'name data) 'face 'bold)
                          (alist-get 'version data)
                          (propertize (alist-get 'license data) 'face 'font-lock-type-face)
                          (propertize (alist-get 'description data) 'face 'font-lock-comment-face)))
          (insert "\nLinks:\n")
          (let* ((homepage (alist-get 'homepage data))
                 (repo-url (string-remove-prefix
                            "git+"
                            (alist-get 'url (alist-get 'repository data)))))
            (jsts--insert-list
             (delq nil
                   (list
                    (when homepage
                      (list (format "%s:" (propertize "Homepage" 'face 'font-lock-keyword-face))
                            (jsts--make-url-button homepage)))
                    (when repo-url
                      (list (format "%s:" (propertize "Repository" 'face 'font-lock-keyword-face))
                            (jsts--make-url-button repo-url)))))))
          (insert "\nTags:\n")
          (let ((tag-list (mapcar
                           (lambda (tag)
                             (let* ((tag-name (symbol-name (car tag)))
                                    (tag-version (cdr tag))
                                    (tag-time (alist-get (intern tag-version)
                                                         (alist-get 'time data))))
                               (list (format "%s:" (propertize tag-name 'face 'font-lock-keyword-face))
                                     tag-version
                                     (format "(%s)" (propertize (car (string-split tag-time "T"))
                                                                'face 'font-lock-comment-face)))))
                           (alist-get 'dist-tags data))))
            (jsts--insert-list
             (cl-sort tag-list #'string> :key (apply-partially #'nth 2))))
          (when-let ((bin-list (alist-get 'bin data)))
            (insert "\nBinaries:\n")
            (dolist (bin-item bin-list)
              (insert (format "- %s\n" (propertize (symbol-name (car bin-item))
                                                   'face 'font-lock-keyword-face)))))
          (when-let ((deps (alist-get 'dependencies data)))
            (jsts--insert-dependency-section "Dependencies" deps))
          (when-let ((deps (alist-get 'peerDependencies data)))
            (jsts--insert-dependency-section "Peer Dependencies" deps))
          (when-let ((deps (alist-get 'optionalDependencies data)))
            (jsts--insert-dependency-section "Optional Dependencies" deps))
          (when-let ((deps (alist-get 'devDependencies data)))
            (jsts--insert-dependency-section "Dev Dependencies" deps))
          (when-let ((deps (alist-get 'bundleDependencies data)))
            (jsts--insert-dependency-section "Bundle Dependencies" deps))
          (buffer-string))))))

;;; * Common transient parts

(defun jsts--exec-suffix ()
  "Common suffix to run data coming from the scope and args"
  (interactive)
  (let* ((tscope (transient-scope))
         (cmd (plist-get tscope :cmd))
         (default-directory (or (plist-get tscope :cwd)
                                default-directory))
         (args (transient-args (oref transient-current-prefix command))))
    (compile (string-join (cons cmd args) " "))))

(defun jsts--exec-and-clear-cache-suffix ()
  "Runs `jsts--exec-suffix' and then clears project cache for :cwd from scope"
  (interactive)
  (jsts--exec-suffix)
  (let ((cwd (or (plist-get (transient-scope) :cwd)
                 default-directory)))
    (jsts--clear-project-cache-for-dir cwd)))

;;; ** jsts--transient-option class

(defclass jsts--transient-option (transient-option)
  ((print-argument :initarg :print-argument :initform t))
  "Class used for arguments that may not print it's argument back on the value")

(cl-defmethod transient-infix-value ((obj jsts--transient-option))
  "Return the value prefixed by its argument when print-argument slot is non-nil"
  (let* ((value (oref obj value))
         (print-arg (oref obj print-argument))
         (arg (when print-arg (oref obj argument))))
    (concat arg value)))

(cl-defmethod transient-format-value ((obj jsts--transient-option))
  "Format the value prefixed by its argument when print-argument slot is non-nil"
  (let* ((value (oref obj value))
         (print-arg (oref obj print-argument))
         (arg (when print-arg (oref obj argument))))
    (concat (when arg (propertize arg 'face (if value 'transient-argument
                                              'transient-inactive-argument)))
            (when value (propertize (prin1-to-string value t)
                                    'face 'transient-value)))))

;;; ** jsts--transient-scope-option class

(defclass jsts--transient-scope-option (jsts--transient-option)
  ((scope-key :initarg :scope-key))
  "Class used for arguments that update a key value in the transient-scope plist")

(cl-defmethod transient-init-value ((obj jsts--transient-scope-option))
  "Extract OBJ's value from the value of the scope-key in transient-scope"
  (oset obj value
        (plist-get (transient-scope) (oref obj scope-key))))

(cl-defmethod transient-infix-set ((obj jsts--transient-scope-option) value)
  "Update value of the scope-key in transient-scope"
  (let* ((scope (transient-scope))
         (key (oref obj scope-key))
         (new-scope (plist-put scope key value)))
    (oset transient--prefix scope new-scope)
    (oset obj value value)))

(cl-defmethod transient-infix-value ((_   jsts--transient-scope-option))
  "Return nil, which means \"no value\" as it has already been set inside
  transient-scope"
  nil)

(transient-define-infix jsts--cwd-infix ()
  "Update the :cwd value in scope"
  :class 'jsts--transient-scope-option
  :scope-key :cwd
  :description "Working directory"
  :prompt "Working directory: "
  :reader #'transient-read-existing-directory
  :argument "$CWD="
  :key "$c"
  :always-read t)

;;; ** Package scripts arguments

(defun jsts--script-completion-table (string predicate action)
  "Completion table for package.json scripts with annotation-function."
  (and-let* (
             (cwd (plist-get (transient-scope) :cwd))
             (project-root (locate-dominating-file cwd "package.json"))
             (pkg-json (jsts--package-json-parse (expand-file-name "package.json" project-root)))
             (scripts (jsts--package-json-get-scripts pkg-json)))
    (cond ((eq action 'metadata)
           `(metadata
             (annotation-function
              . ,(lambda (s)
                   (let* ((pair (assoc-string s scripts))
                          (script-name (car pair))
                          (script-cmd  (cdr pair))
                          (all-script-names (mapcar #'car scripts))
                          (max-script-width (apply #'max (mapcar #'string-width all-script-names)))
                          (padding (- (+ 5 max-script-width)
                                      (string-width script-name))))
                     (concat (make-string padding ?\s)
                             (propertize script-cmd 'face 'font-lock-doc-face)))))))
          (t
           (complete-with-action action (mapcar #'car scripts) string predicate)))))

(transient-define-argument jsts--script-arg ()
  "Name of the script to run."
  :class 'jsts--transient-option
  :description "Script"
  :key " s"
  :prompt "Script: "
  :choices (lambda () #'jsts--script-completion-table)
  :print-argument nil
  :argument ":script=" ; this won't get printed, but it's useful when initializing a prefix
  :always-read t
  :allow-empty nil)

(transient-define-argument jsts--script-extra-args-arg ()
  "Extra positional arguments to send to the script."
  :class 'jsts--transient-option
  :description "Script arguments"
  :key "--"
  :prompt "Script arguments: "
  :argument "-- "
  :always-read t
  :allow-empty nil)

;;; * Package managers transient menus

;;; ** npm

(transient-define-argument jsts--npm-install-save-arg ()
  "Choices for how to save a package"
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(save\\|no-save\\|save-dev\\|save-prod\\|save-optional\\|save-peer\\|save-bundle\\)\\)"
  :choices '("save" "no-save" "save-dev" "save-prod" "save-optional" "save-peer" "save-bundle")
  :init-value (lambda (obj) (oset obj value "--save")))

(transient-define-argument jsts--npm-install-packages-arg ()
  "List of packages to be installed"
  :class 'transient-option
  :prompt "Package spec(s): "
  :argument ""
  :always-read t
  :allow-empty nil)

;;;###autoload
(transient-define-prefix jsts-npm-install ()
  "Display npm install commands"
  ["npm install\n"
   ("  s" "Save" jsts--npm-install-save-arg)
   (" -E" "Save exact" "--save-exact")
   (" -I" "Ignore scripts" "--ignore-scripts")
   " "
   ("  p" "Package spec(s)" jsts--npm-install-packages-arg)
   " "
   ("RET" "Install" jsts--exec-suffix)]
  (interactive)
  (transient-setup 'jsts-npm-install nil nil
                   :scope `(:cwd ,(jsts-ensure-project)
                            :cmd "npm install")))

;;;###autoload
(transient-define-prefix jsts-npm-run-script (&optional script cwd)
  "Display npm run commands

SCRIPT (optional): The npm script to run. If non-nil, it will be pre-selected in
the menu.
CWD (optional): The directory in which to run the npm command. If nil, the
current project root is used."
  ["npm run"
   :pad-keys t
   (jsts--cwd-infix)
   " "
   ("-I" "Ignore scripts" "--ignore-scripts")
   " "
   (jsts--script-arg)
   (jsts--script-extra-args-arg)
   " "
   ("RET" "Run" jsts--exec-suffix)]
  (interactive)
  (let ((project-root (jsts-ensure-project cwd)))
    (transient-setup 'jsts-npm-run-script nil nil
                     :value (list (when script (concat ":script=" script)))
                     :scope (list :cwd (or cwd project-root)
                                  :cmd "npm run"))))

;;;###autoload
(transient-define-prefix jsts-npm-init ()
  "Display npm init command"
  ["npm init\n"
   :class transient-column
   :pad-keys t
   ("w" "Workspace" "--workspace=" :reader transient-read-directory)
   ("n" "Author Name" "--init-author-name=")
   ("u" "Author URL" "--init-author-url=")
   ;; TODO learn how to provide async choices and then prompt the user to call
   ;; `jsts-update-licenses-cache' automatically
   ("l" "License" "--init-license=" :choices (lambda () (jsts-osi-licenses)))
   ("t" "Type (module system)" "--init-type=" :choices ("module" "commonjs"))
   ("v" "Version" "--init-version=")
   ("p" "Private" "--init-private")
   " "
   ("RET" "Run" jsts--exec-and-clear-cache-suffix)]
  (interactive)
  (transient-setup 'jsts-npm-init nil nil
                   ;; we don't call `jsts-ensure-project' as init may be called
                   ;; to create a project
                   :scope `(:cwd ,(jsts-project-root)
                            :cmd "npm init --yes")))

;;;###autoload
(transient-define-prefix jsts-npm ()
  "Display npm commands"
  ["npm\n"
   ("i" "Install" jsts-npm-install :if jsts-project-root)
   ("I" "Init" jsts-npm-init)
   ("s" "Run script" jsts-npm-run-script :if jsts-project-root)
   ("v" "View package info" jsts-view-package)]
  (interactive)
  (transient-setup 'jsts-npm nil nil
                   :scope `(:cwd ,(jsts-project-root))))

;;; ** bun

;;;###autoload
(transient-define-prefix jsts-bun-run-script (&optional script cwd)
  "Display bun run commands

SCRIPT (optional): The bun script to run. If non-nil, it will be pre-selected in
the menu.
CWD (optional): The directory in which to run the bun command. If nil, the
current project root is used."
  ["bun run"
   :pad-keys t
   (jsts--cwd-infix)
   " "
   ("-b" "Force Bun runtime" "--bun")
   ("-w" "Restart process on file change" "--watch")
   ("-h" "Hot reload" "--hot")
   " "
   (jsts--script-arg)
   (jsts--script-extra-args-arg :print-argument nil)
   " "
   ("RET" "Run" jsts--exec-suffix)]
  (interactive)
  (let ((project-root (jsts-ensure-project cwd)))
    (transient-setup 'jsts-bun-run-script nil nil
                     :value (list (when script (concat ":script=" script)))
                     :scope (list :cwd (or cwd project-root)
                                  :cmd "bun run"))))

;;;###autoload
(transient-define-prefix jsts-bun-init ()
  "Display bun init command"
  :incompatible '(("--minimal" "--react" "--react=tailwind" "--react=shadcn"))
  ["bun init\n"
   :class transient-column
   :pad-keys t
   ("m"  "Only initialize type definitions" "--minimal")
   ("rr" "Initialize a React project" "--react")
   ("rt" "Initialize a React project with TailwindCSS" "--react=tailwind")
   ("rs" "Initialize a React project with @shadcn/ui and TailwindCSS" "--react=shadcn")
   " "
   ("RET" "Run" jsts--exec-and-clear-cache-suffix)]
  (interactive)
  (transient-setup 'jsts-bun-init nil nil
                   ;; we don't call `jsts-ensure-project' as init may be called
                   ;; to create a project
                   :scope `(:cwd ,(jsts-project-root)
                            :cmd "bun init --yes")))

;;;###autoload
(transient-define-prefix jsts-bun ()
  "Display bun commands"
  ["bun\n"
   ("I" "Init" jsts-bun-init)
   ("s" "Run script" jsts-bun-run-script :if jsts-project-root)
   ("v" "View package info" jsts-view-package)]
  (interactive)
  (transient-setup 'jsts-bun nil nil
                   :scope `(:cwd ,(jsts-project-root))))

;;; ** deno

;;;###autoload
(transient-define-prefix jsts-deno-init ()
  "Display deno init command"
  :incompatible '(("--lib" "--serve"))
  ["deno init\n"
   :class transient-column
   :pad-keys t
   ("l" "Generate an example library project" "--lib")
   ("s" "Generate an example project for `deno serve`" "--serve")
   " "
   ("RET" "Run" jsts--exec-and-clear-cache-suffix)]
  (interactive)
  (transient-setup 'jsts-deno-init nil nil
                   ;; we don't call `jsts-ensure-project' as init may be called
                   ;; to create a project
                   :scope `(:cwd ,(jsts-project-root)
                            :cmd "deno init")))

;;;###autoload
(transient-define-prefix jsts-deno ()
  "Display deno commands"
  ["deno\n"
   ("I" "Init" jsts-deno-init)]
  (interactive)
  (transient-setup 'jsts-deno nil nil
                   :scope `(:cwd ,(jsts-project-root))))

;;; ** pnpm

;;;###autoload
(transient-define-prefix jsts-pnpm-run-script (&optional script cwd)
  "Display pnpm run commands

SCRIPT (optional): The pnpm script to run. If non-nil, it will be pre-selected
in the menu.
CWD (optional): The directory in which to run the pnpm command. If nil, the
current project root is used."
  ["pnpm run"
   :pad-keys t
   (jsts--cwd-infix)
   " "
   (jsts--script-arg)
   (jsts--script-extra-args-arg :print-argument nil)
   " "
   ("RET" "Run" jsts--exec-suffix)]
  (interactive)
  (let ((project-root (jsts-ensure-project cwd)))
    (transient-setup 'jsts-pnpm-run-script nil nil
                     :value (list (when script (concat ":script=" script)))
                     :scope (list :cwd (or cwd project-root)
                                  :cmd "pnpm run"))))

;;;###autoload
(transient-define-prefix jsts-pnpm-init ()
  "Display pnpm init command"
  ["pnpm init\n"
   :class transient-column
   :pad-keys t
   ("p" "Pin current pnpm version" "--init-package-manager")
   ("t" "Type (module system)" "--init-type=" :choices ("module" "commonjs"))
   " "
   ("RET" "Run" jsts--exec-and-clear-cache-suffix)]
  (interactive)
  (transient-setup 'jsts-pnpm-init nil nil
                   ;; we don't call `jsts-ensure-project' as init may be called
                   ;; to create a project
                   :scope `(:cwd ,(jsts-project-root)
                            :cmd "pnpm init")))

;;;###autoload
(transient-define-prefix jsts-pnpm ()
  "Display pnpm commands"
  ["pnpm\n"
   ("I" "Init" jsts-pnpm-init)
   ("s" "Run script" jsts-pnpm-run-script :if jsts-project-root)
   ("v" "View package info" jsts-view-package)]
  (interactive)
  (transient-setup 'jsts-pnpm nil nil
                   :scope `(:cwd ,(jsts-project-root))))

;;; ** yarn

;;;###autoload
(transient-define-prefix jsts-yarn-run-script (&optional script cwd)
  "Display yarn run commands

SCRIPT (optional): The yarn script to run. If non-nil, it will be pre-selected in
the menu.
CWD (optional): The directory in which to run the yarn command. If nil, the
current project root is used."
  ["yarn run"
   :pad-keys t
   (jsts--cwd-infix)
   " "
   (jsts--script-arg)
   (jsts--script-extra-args-arg)
   " "
   ("RET" "Run" jsts--exec-suffix)]
  (interactive)
  (let ((project-root (jsts-ensure-project cwd)))
    (transient-setup 'jsts-yarn-run-script nil nil
                     :value (list (when script (concat ":script=" script)))
                     :scope (list :cwd (or cwd project-root)
                                  :cmd "yarn run"))))

;;;###autoload
(transient-define-prefix jsts-yarn-init ()
  "Display yarn init command"
  ["yarn init\n"
   :class transient-column
   :pad-keys t
   ("p" "Private" "--private")
   " "
   ("RET" "Run" jsts--exec-and-clear-cache-suffix)]
  (interactive)
  (transient-setup 'jsts-yarn-init nil nil
                   ;; we don't call `jsts-ensure-project' as init may be called
                   ;; to create a project
                   :scope `(:cwd ,(jsts-project-root)
                            :cmd "yarn init --yes")))

;;;###autoload
(transient-define-prefix jsts-yarn ()
  "Display yarn commands"
  ["yarn\n"
   ("I" "Init" jsts-yarn-init)
   ("s" "Run script" jsts-yarn-run-script :if jsts-project-root)
   ("v" "View package info" jsts-view-package)]
  (interactive)
  (transient-setup 'jsts-yarn nil nil
                   :scope `(:cwd ,(jsts-project-root))))

;;; * jsts entrypoint

(defun jsts-package-manager (&optional project)
  "Return package manager name.
If PROJECT is not specified acts on the current project."
  (and-let* ((project-root (or project (jsts-project-root))))
    (cl-some (lambda (pair)
               (let ((lockfile (car pair))
                     (manager (cdr pair)))
                 (when (file-exists-p (expand-file-name lockfile project-root))
                   manager)))
             jsts-lockfile-to-manager-alist)))

(defun jsts-read-package-manager ()
  "Read a JS/TS package manager."
  (let ((managers (mapcar (apply-partially #'cdr) jsts-lockfile-to-manager-alist)))
    (completing-read "Package manager: " managers)))

;;;autoload
(defun jsts-init ()
  "Initialize a JS/TS project using a chosen package manager."
  (interactive)
  (let* ((pm (jsts-read-package-manager))
         (init-func (intern (format "jsts-%s-init" pm))))
    (when (not (fboundp init-func))
      (user-error "%s has no init function defined yet." pm))
    (let ((default-directory (transient-read-directory "Initialize project in: " nil nil)))
      (funcall init-func))))

;;;###autoload
(defun jsts (&optional cmd &rest args)
  "Begin using jsts"
  (interactive)
  (if (and (not (jsts-project-root))
           (not cmd))
      (jsts-init)
    (let* ((pm (or (jsts-package-manager)
                   (jsts-read-package-manager)))
           (pm-func (intern (if cmd
                                (format "jsts-%s-%s" pm cmd)
                              (format "jsts-%s" pm)))))
      (if (fboundp pm-func) (apply pm-func args)
        (message "%s is not yet supported." pm)))))

(provide 'jsts)
;;; jsts.el ends here
