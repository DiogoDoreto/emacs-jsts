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
(require 'button)

(defgroup jsts-package-json-mode nil
  "Enhancements for package.json files."
  :group 'tools)

(defun jsts-package-json--button-action (button)
  "Placeholder action for dependency BUTTON."
  (message "Clicked on dependency: %s" (button-label button)))

(defun jsts-package-json--buttonize-dependencies ()
  "Find all dependency names in package.json and make them text buttons."
  (interactive)
  (when (not (derived-mode-p 'json-ts-mode))
    (user-error "Only json-ts-mode is supported."))
  (let* ((query '((pair
                   key: (string) @dep-key
                   value: (object
                           (pair
                            key: (string) @pkg-key
                            value: (string))
                           :+)
                   (:match "^\"\\(dependencies\\|devDependencies\\|peerDependencies\\)\"$" @dep-key))))
         (matches (treesit-query-capture 'json query)))
    (save-excursion
      (remove-text-properties (point-min) (point-max) '(jsts-pkg-dep t))
      (dolist (match matches)
        (let ((capture (car match))
              (node (cdr match)))
          (when (string= capture "pkg-key")
            (let* ((start (treesit-node-start node))
                   (end (treesit-node-end node))
                   (label (buffer-substring-no-properties (1+ start) (1- end))))
              (goto-char start)
              (when (not (get-text-property start 'jsts-pkg-dep))
                (make-text-button (1+ start) (1- end)
                                  'type 'jsts-pkg-dep-button
                                  'jsts-pkg-dep t
                                  'help-echo "Dependency name"
                                  'action #'jsts-package-json--button-action)))))))))

(define-button-type 'jsts-pkg-dep-button
  'follow-link t
  'face 'link)

;;;###autoload
(define-minor-mode jsts-package-json-mode
  "Minor mode for enhancing package.json files."
  :lighter " JSTSPkg"
  (if jsts-package-json-mode
      (jsts-package-json--buttonize-dependencies)
    (remove-text-properties (point-min) (point-max) '(jsts-pkg-dep t))))

;; (add-hook 'json-mode-hook #'jsts-package-json-mode)
(add-hook 'json-ts-mode-hook #'jsts-package-json-mode)

(provide 'jsts-package-json-mode)
;;; jsts-package-json-mode.el ends here
