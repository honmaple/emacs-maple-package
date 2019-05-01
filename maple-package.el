;;; maple-package.el ---  package configuration.    -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
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

(defvar maple-package-autoload-file (expand-file-name "cache/autoloads.pkg.el" user-emacs-directory))

(defun maple-package-upgrade-alist(&optional filterp)
  "Upgrade package alist with FILTERP."
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if filterp
        (let ((v (mapcar (lambda(name) (list (package-desc-full-name name) name)) upgrades)))
          (cdr (assoc (completing-read "Upgrade package: " v) v)))
      upgrades)))

(defun maple-package-upgrade-by-name()
  "Upgrade packages with SELECTED."
  (interactive)
  (maple-package-upgrade t))

(defun maple-package-upgrade (&optional filterp)
  "Upgrade all packages automatically without showing *Packages* buffer with FILTERP."
  (interactive)
  (let ((upgrades (maple-package-upgrade-alist filterp)))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))


;; https://emacs-china.org/t/advice/7566
(defun maple-package-function-advices (function)
  "Return FUNCTION's advices."
  (let ((function-def (advice--symbol-function function))
        (ad-functions '()))
    (while (advice--p function-def)
      (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
      (setq function-def (advice--cdr function-def)))
    ad-functions))

(define-advice describe-function-1 (:after (function) advice-remove-button)
  "Add a button to remove advice."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        (let ((ad-index 0)
              (ad-list (reverse (maple-package-function-advices function))))
          (while (re-search-forward "^:[-a-z]+ advice: \\(.+\\)$" nil t)
            (let* ((name (string-trim (match-string 1) "'" "'"))
                   (advice (or (intern-soft name) (nth ad-index ad-list))))
              (when (and advice (functionp advice))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                   'help-echo (format "%s" advice)
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (when (yes-or-no-p (format "Remove %s ? " ',advice))
                        (message "Removing %s of advice from %s" ',function ',advice)
                        (advice-remove ',function ',advice)
                        ;; (revert-buffer nil t)
                        ))
                   'follow-link t))))
            (setq ad-index (1+ ad-index))))))))

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
      (dolist (spec (maple-package-alist))
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

(defun maple-package-alist ()
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
    (condition-case nil
        (when (byte-compile-file file)
          (load (byte-compile-dest-file file) nil t)
          (unless noninteractive
            (message "Finished compiling %s" short-name)))
      ((debug error)
       (ignore-errors (delete-file (byte-compile-dest-file file)))))))

(defun maple-package-initialize-autoload(&optional force)
  "Initialize autoload file with FORCE."
  (let ((dest-file (byte-compile-dest-file maple-package-autoload-file)))
    (when (or force (not (file-exists-p maple-package-autoload-file)))
      (maple-package-reload-autoload))
    (when (or force (file-newer-than-file-p maple-package-autoload-file dest-file))
      (maple-package-byte-compile-file maple-package-autoload-file))
    (load dest-file nil t)))

(defun maple-package-force-initialize()
  "Force initialize package."
  (interactive)
  (when (file-exists-p maple-package-autoload-file)
    (delete-file maple-package-autoload-file))
  (maple-package-initialize-autoload t))

;;;###autoload
(defun maple-package-initialize(&optional no-activate)
  "Package initialize with NO-ACTIVATE."
  (if (not (file-exists-p maple-package-autoload-file))
      (package-initialize)
    (setq package-alist nil)
    (package-load-all-descriptors)
    ;; (package-read-all-archive-contents)
    (unless no-activate
      (dolist (elt package-alist)
        (condition-case err
            (package-activate (car elt))
          (error (message "%s" (error-message-string err))))))
    (setq package--initialized t)
    (package--build-compatibility-table))
  (when no-activate (maple-package-initialize-autoload)))

(provide 'maple-package)
;;; maple-package.el ends here
