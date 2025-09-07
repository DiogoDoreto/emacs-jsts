;;; jsts-package-json-mode.el --- Minor mode for enhancing package.json files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Diogo Doreto

;; This program is free software: you can redistribute it and/or modify
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

;;  Minor mode for enhancing package.json files

;;; Code:
(require 'treesit)
(require 'jsts)

(defgroup jsts-package-json-mode nil
  "Enhancements for package.json files."
  :group 'tools)

(defun jsts-package-json--button-action (button)
  "Placeholder action for dependency BUTTON."
  (jsts-view-package (button-label button)))

(defun jsts-package-json--buttonize-dependencies ()
  "Find all dependency names in package.json and make them text buttons."
  (interactive)
  (when (not (derived-mode-p 'json-ts-mode))
    (user-error "Only json-ts-mode is supported."))
  (let* ((dep-key-regexp (rx-let ((dep-keys (or "dependencies" "devDependencies" "peerDependencies" "optionalDependencies")))
                           (rx bol ?\" dep-keys ?\" eol)))
         (query `((pair
                   key: (string) @dep-key
                   value: (object (pair
                                   key: (string) @pkg-key
                                   value: (string)))
                   (:match ,dep-key-regexp @dep-key))))
         (matches (treesit-query-capture 'json query)))
    (dolist (match matches)
      (let ((capture (car match))
            (node (cdr match)))
        (when (string= capture "pkg-key")
          ;; we bind start and end moving 1 char to ignore the quotes
          (let* ((start (1+ (treesit-node-start node)))
                 (end (1- (treesit-node-end node)))
                 (pkg-name (buffer-substring-no-properties start end))
                 (existing-button (button-at start)))
            (unless existing-button
              (make-button start end
                           'type 'jsts-pkg-dep-button
                           'help-echo (format "View %s package information" pkg-name)
                           'action #'jsts-package-json--button-action))))))))

(define-button-type 'jsts-pkg-dep-button
  'jsts-pkg-dep t
  'follow-link t
  'face 'underline)

;;;###autoload
(defun jsts-package-json--after-change (_beg _end _len)
  (when jsts-package-json-mode
    (jsts-package-json--buttonize-dependencies)))

(define-minor-mode jsts-package-json-mode
  "Minor mode for enhancing package.json files."
  :lighter " JSTSPkg"
  (if jsts-package-json-mode
      (progn
        (add-hook 'after-change-functions #'jsts-package-json--after-change nil t)
        (jsts-package-json--buttonize-dependencies))
    (remove-hook 'after-change-functions #'jsts-package-json--after-change t)
    (remove-overlays (point-min) (point-max) 'jsts-pkg-dep t)))

(add-hook 'json-ts-mode-hook #'jsts-package-json-mode)

(provide 'jsts-package-json-mode)
;;; jsts-package-json-mode.el ends here
