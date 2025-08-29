;;; jsts.el --- JS/TS tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Diogo Doreto
;;
;; Author: Diogo Doreto <diogo@doreto.com.br>
;; Version: 0.0.1
;; Keywords: convenience tools javascript typescript
;; URL: https://github.com/DiogoDoreto/emacs-jsts
;; Package-Requires: ((emacs "24.3"))
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

;;; Declarations
;;
;; A bunch of variable and function declarations
;; needed to appease the byte-compiler.
(declare-function tramp-archive-file-name-p "tramp-archive")

;;; Customization
(defgroup jsts nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/DiogoDoreto/emacs-jsts"))

(defcustom jsts-lockfile-to-manager-alist
  '(("package-lock.json" . npm)
    ("bun.lock"          . bun)
    ("bun.lockb"         . bun)
    ("yarn.lock"         . yarn)
    ("pnpm-lock.yaml"    . pnpm))
  "Alist mapping lockfile file names to their respective package manager."
  :type  '(alist :key-type string :value-type symbol)
  :group 'jsts)

(defcustom jsts-default-manager 'npm
  "Default package manager to use if no lockfile is found."
  :type  'symbol
  :group 'jsts)

;;; Project root
;;
;; Heavily inspired by how Projectile manages its projects
(defvar jsts-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `jsts-project-root'.")

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

;;; package.json

(defun jsts--package-json-parse (file)
  (let ((json-object-type 'alist)
        (json-key-type 'string)
        (json-array-type 'list))
    (ignore-errors (json-read-file file))))

(defun jsts--package-json-get-name (parsed-file)
  (alist-get "name" parsed-file nil nil #'string=))

(defun jsts--package-json-get-scripts (parsed-file)
  (alist-get "scripts" parsed-file nil nil #'string=))

;;; npm

(transient-define-suffix jsts--npm-install-suffix ()
  (interactive)
  (let ((default-directory (jsts-ensure-project (transient-scope)))
        (args (transient-args (oref transient-current-prefix command))))
    (compile (format "npm install %s" (string-join args " ")))))

(transient-define-argument jsts--npm-install-save-arg ()
  "Choices for how to save a package"
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(\\.\\+\\)\\)"
  :choices '("save" "no-save" "save-dev" "save-prod" "save-optional" "save-peer" "save-bundle"))

(transient-define-argument jsts--npm-install-packages-arg ()
  "List of packages to be installed"
  :class 'transient-option
  :prompt "Package spec(s): "
  :argument ""
  :always-read t
  :allow-empty nil)

(transient-define-prefix jsts-npm-install ()
  "Display npm install commands"
  ["npm install\n"
   ("  s" "Save" jsts--npm-install-save-arg)
   (" -E" "Save exact" "--save-exact")
   (" -I" "Ignore scripts" "--ignore-scripts")
   " "
   ("  p" "Package spec(s)" jsts--npm-install-packages-arg)
   " "
   ("RET" "Install" jsts--npm-install-suffix)]
  (interactive)
  (transient-setup 'jsts-npm-install nil nil :scope (jsts-ensure-project)))

(defun jsts--npm-script-completion-table (string predicate action)
  "Completion table for package.json scripts with annotation-function."
  (and-let* ((project-root (jsts-ensure-project (transient-scope)))
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

(transient-define-argument jsts--npm-script-arg ()
  "Name of the script to run."
  :class 'transient-option
  :prompt "Script: "
  :choices (lambda () #'jsts--npm-script-completion-table)
  :argument ""
  :always-read t
  :allow-empty nil)

(transient-define-argument jsts--npm-script-args-arg ()
  "Extra positional arguments to send to the script."
  :class 'transient-option
  :prompt "Script arguments: "
  :argument "-- "
  :always-read t
  :allow-empty nil)

(transient-define-suffix jsts--npm-run-script-suffix ()
  (interactive)
  (let ((default-directory (jsts-ensure-project (transient-scope)))
        (args (transient-args (oref transient-current-prefix command))))
    (compile (format "npm run %s" (string-join args " ")))))

(transient-define-prefix jsts-npm-run-script ()
  "Display npm run commands"
  ["npm run\n"
   (" -I" "Ignore scripts" "--ignore-scripts")
   " "
   ("  s" "Script" jsts--npm-script-arg)
   (" --" "Script arguments" jsts--npm-script-args-arg)
   " "
   ("RET" "Run" jsts--npm-run-script-suffix)]
  (interactive)
  (transient-setup 'jsts-npm-run-script nil nil :scope (jsts-ensure-project)))

(transient-define-prefix jsts-npm ()
  "Display npm commands"
  ["npm\n"
   ("i" "Install" jsts-npm-install)
   ("s" "Run script" jsts-npm-run-script)]
  (interactive)
  (transient-setup 'jsts-npm nil nil :scope (jsts-ensure-project)))

;;; jsts entrypoint

(defun jsts-package-manager (&optional project)
  "Return package manager name.
If PROJECT is not specified acts on the current project."
  (and-let* ((project-root (or project (jsts-project-root))))
    (or (cl-some (lambda (pair)
                   (let ((lockfile (car pair))
                         (manager (cdr pair)))
                     (when (file-exists-p (expand-file-name lockfile project-root))
                       manager)))
                 jsts-lockfile-to-manager-alist)
        jsts-default-manager)))

(defun jsts ()
  "Begin using jsts"
  (interactive)
  (let ((pm (jsts-package-manager (jsts-ensure-project))))
    (cond ((eq pm 'npm) (jsts-npm))
          (t (message "%s is not yet supported" pm)))))

(provide 'jsts)
;;; jsts.el ends here
