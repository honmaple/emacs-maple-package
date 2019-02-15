;;; maple-package.el ---  package configuration.    -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; package configuration.
;;

;;; Code:
(require 'package)

(defvar maple-package-autoload-file (concat user-emacs-directory "cache/autoloads.pkg.el"))

(defun maple-package-reload-autoload ()
  "Generate autoload file."
  (with-temp-file maple-package-autoload-file
    (insert ";; -*- lexical-binding:t -*-\n")
    (prin1 `(setq load-path ',load-path
                  auto-mode-alist ',auto-mode-alist
                  package-activated-list ',package-activated-list)
           (current-buffer))
    (insert "\n\n")
    (save-excursion
      (dolist (spec (maple-package-package-alist))
        (let* ((desc (cdr spec))
               (file (concat (package--autoloads-file-name desc) ".el")))
          (when (file-exists-p file)
            (insert "(let ((load-file-name " (prin1-to-string (abbreviate-file-name file)) "))\n")
            (insert-file-contents file)
            (while (re-search-forward "^\\(?:;;\\(.*\n\\)\\|\n\\|(provide '[^\n]+\\)" nil t)
              (unless (nth 8 (syntax-ppss))
                (replace-match "" t t)))
            (unless (bolp) (insert "\n"))
            (insert ")\n")))))
    (while (re-search-forward "^\\s-*\\((\\(?:add-to-list\\|\\(?:when\\|if\\) (boundp\\)\\s-+'\\(?:load-path\\|auto-mode-alist\\)\\)" nil t)
      (goto-char (match-beginning 1))
      (kill-sexp))))

(defun maple-package-package-alist ()
  "Return package alist."
  (cl-remove-duplicates
   (cl-loop for name in (mapcar #'car package-alist)
            if (assq name package-alist)
            nconc (cl-loop for dep in (package--get-deps name)
                           if (assq dep package-alist)
                           collect (cons dep (cadr it)))
            and collect (cons name (cadr it)))
   :key #'car
   :from-end t))

(defun maple-package-byte-compile-file (file)
  "Bytes compile FILE."
  (let ((short-name (file-name-nondirectory file))
        (byte-compile-dynamic-docstrings t))
    (when (byte-compile-file file)
      (load (byte-compile-dest-file file) nil t)
      (unless noninteractive
        (message "Finished compiling %s" short-name)))))

(defun maple-package-initialize-autoload()
  "Initialize autoload file."
  (when (not (file-exists-p maple-package-autoload-file))
    (maple-package-reload-autoload))
  (when (file-newer-than-file-p maple-package-autoload-file
                                (byte-compile-dest-file maple-package-autoload-file))
    (maple-package-byte-compile-file maple-package-autoload-file))
  (load (byte-compile-dest-file maple-package-autoload-file) nil t))

;;;###autoload
(defun maple-package-initialize(&optional no-activate)
  "Initialize NO-ACTIVATE."
  (when (not (file-exists-p maple-package-autoload-file))
    (package-initialize))
  (setq package-alist nil)
  (package-load-all-descriptors)
  ;; (package-read-all-archive-contents)
  (unless no-activate
    (dolist (elt package-alist)
      (condition-case err
          (package-activate (car elt))
        (error (message "%s" (error-message-string err))))))
  (setq package--initialized t)
  (package--build-compatibility-table)
  (when no-activate (maple-package-initialize-autoload)))

(provide 'maple-package)
;;; maple-package.el ends here
