;;; rubocop.el --- An Emacs interface for RuboCop -*- lexical-binding: t -*-

;; Copyright © 2011-2021 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/rubocop/rubocop-emacs
;; Version: 0.7.0-snapshot
;; Keywords: project, convenience
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;; Commentary:
;;
;; This library allows the user to easily invoke RuboCop to get feedback
;; about stylistic issues in Ruby code.  It also gives access to RuboCop
;; auto-correction functionality.
;;
;;; Code:

(require 'tramp)

(defgroup rubocop nil
  "An Emacs interface for RuboCop."
  :group 'tools
  :group 'convenience
  :prefix "rubocop-"
  :link '(url-link :tag "GitHub" "https://github.com/rubocop/rubocop-emacs"))

(defcustom rubocop-project-root-files
  '(".projectile" ".git" ".hg" ".bzr" "_darcs" "Gemfile")
  "A list of files considered to mark the root of a project."
  :type '(repeat string))

(defcustom rubocop-check-command
  "rubocop --format emacs"
  "The command used to run RuboCop checks."
  :type 'string)

(defcustom rubocop-autocorrect-command
  "rubocop -a --format emacs"
  "The command used to run RuboCop's autocorrection."
  :type 'string)

(defcustom rubocop-format-command
  "rubocop -x --format emacs"
  "The command used to run RuboCop's code formatting.
It's basically auto-correction limited to layout cops."
  :type 'string
  :package-version '(rubocop . "0.6.0"))

(defcustom rubocop-extensions
  '()
  "A list of extensions to be loaded by RuboCop."
  :type '(repeat string))

(defcustom rubocop-keymap-prefix (kbd "C-c C-r")
  "RuboCop keymap prefix."
  :group 'rubocop
  :type 'string)

(defcustom rubocop-autocorrect-on-save nil
  "Runs `rubocop-autocorrect-current-file' automatically on save."
  :group 'rubocop
  :type 'boolean
  :package-version '(rubocop . "0.6.0"))

(defcustom rubocop-format-on-save nil
  "Runs `rubocop-format-current-file' automatically on save."
  :group 'rubocop
  :type 'boolean
  :package-version '(rubocop . "0.7.0"))

(defcustom rubocop-prefer-system-executable nil
  "Runs rubocop with the system executable even if inside a bundled project."
  :group 'rubocop
  :type 'boolean)

(defcustom rubocop-run-in-chroot nil
  "Runs rubocop inside a chroot via schroot setting the cwd to the project's root."
  :group 'rubocop
  :type 'boolean
  :package-version '(rubocop . "0.7.0"))

(defun rubocop-local-file-name (file-name)
  "Retrieve local filename if FILE-NAME is opened via TRAMP."
  (cond ((tramp-tramp-file-p file-name)
         (tramp-file-name-localname (tramp-dissect-file-name file-name)))
        (t
         file-name)))

(defun rubocop-project-root (&optional no-error)
  "Retrieve the root directory of a project if available.

When NO-ERROR is non-nil returns nil instead of raise an error."
  (or
   (car
    (mapcar #'expand-file-name
            (delq nil
                  (mapcar
                   (lambda (f) (locate-dominating-file default-directory f))
                   rubocop-project-root-files))))
   (if no-error
       nil
     (error "You're not into a project"))))

(defun rubocop-buffer-name (file-or-dir)
  "Generate a name for the RuboCop buffer from FILE-OR-DIR."
  (concat "*RuboCop " file-or-dir "*"))

(defun rubocop-build-requires ()
  "Build RuboCop requires from `rubocop-extensions'."
  (if rubocop-extensions
      (concat
       " "
       (mapconcat
        (lambda (ext)
          (format "--require %s" ext))
        rubocop-extensions
        " ")
       " ")
    ""))

(defun rubocop-build-command (command path)
  "Build the full command to be run based on COMMAND and PATH.
The command will be prefixed with `bundle exec` if RuboCop is bundled."
  (concat
   (if rubocop-run-in-chroot (format "schroot -d %s -- " (rubocop-project-root)))
   (if (and (not rubocop-prefer-system-executable) (rubocop-bundled-p)) "bundle exec " "")
   command
   (rubocop-build-requires)
   " "
   path))

(defun rubocop--dir-command (command &optional directory)
  "Run COMMAND on DIRECTORY (if present).
Alternatively prompt user for directory."
  (rubocop-ensure-installed)
  (let ((directory
         (or directory
             (read-directory-name "Select directory: "))))
    ;; make sure we run RuboCop from a project's root if the command is executed within a project
    (let ((default-directory (or (rubocop-project-root 'no-error) default-directory)))
      (compilation-start
       (rubocop-build-command command (rubocop-local-file-name directory))
       'compilation-mode
       (lambda (arg) (message arg) (rubocop-buffer-name directory))))))

;;;###autoload
(defun rubocop-check-project ()
  "Run check on current project."
  (interactive)
  (rubocop-check-directory (rubocop-project-root)))

;;;###autoload
(defun rubocop-autocorrect-project ()
  "Run autocorrect on current project."
  (interactive)
  (rubocop-autocorrect-directory (rubocop-project-root)))

;;;###autoload
(defun rubocop-format-project ()
  "Run format on current project."
  (interactive)
  (rubocop-format-directory (rubocop-project-root)))

;;;###autoload
(defun rubocop-check-directory (&optional directory)
  "Run check on DIRECTORY if present.
Alternatively prompt user for directory."
  (interactive)
  (rubocop--dir-command rubocop-check-command directory))

;;;###autoload
(defun rubocop-autocorrect-directory (&optional directory)
  "Run autocorrect on DIRECTORY if present.
Alternatively prompt user for directory."
  (interactive)
  (rubocop--dir-command rubocop-autocorrect-command directory))

(defun rubocop-format-directory (&optional directory)
  "Run format on DIRECTORY if present.
Alternatively prompt user for directory."
  (interactive)
  (rubocop--dir-command rubocop-format-command directory))

(defun rubocop--file-command (command)
  "Run COMMAND on currently visited file."
  (rubocop-ensure-installed)
  (let ((file-name (buffer-file-name (current-buffer))))
    (if file-name
        ;; make sure we run RuboCop from a project's root if the command is executed within a project
        (let ((default-directory (or (rubocop-project-root 'no-error) default-directory)))
          (compilation-start
           (rubocop-build-command command (rubocop-local-file-name file-name))
           'compilation-mode
           (lambda (_arg) (rubocop-buffer-name file-name))))
      (error "Buffer is not visiting a file"))))

;;;###autoload
(defun rubocop-check-current-file ()
  "Run check on current file."
  (interactive)
  (rubocop--file-command rubocop-check-command))

;;;###autoload
(defun rubocop-autocorrect-current-file ()
  "Run autocorrect on current file."
  (interactive)
  (rubocop--file-command rubocop-autocorrect-command))

(defun rubocop-autocorrect-current-file-silent ()
  "This command is used by the minor mode to auto-correct on save.
See also `rubocop-autocorrect-on-save'."
  (when rubocop-autocorrect-on-save
    (save-window-excursion (rubocop-autocorrect-current-file))))

;;;###autoload
(defun rubocop-format-current-file ()
  "Run format on current file."
  (interactive)
  (rubocop--file-command rubocop-format-command))

(defun rubocop-format-current-file-silent ()
  "This command is used by the minor mode to format on save.
See also `rubocop-format-on-save' and `rubocop-autocorrect-on-save'."
  (when (and rubocop-format-on-save (not rubocop-autocorrect-on-save))
    (save-window-excursion (rubocop-format-current-file))))

(defun rubocop-bundled-p ()
  "Check if RuboCop has been bundled."
  (let ((gemfile-lock (expand-file-name "Gemfile.lock" (rubocop-project-root))))
    (when (file-exists-p gemfile-lock)
      (with-temp-buffer
        (insert-file-contents gemfile-lock)
        (re-search-forward "rubocop" nil t)))))

(defun rubocop-ensure-installed ()
  "Check if RuboCop is installed."
  (unless (or (executable-find "rubocop") (rubocop-bundled-p))
    (error "RuboCop is not installed")))

;;; Minor mode
(defvar rubocop-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "p") #'rubocop-check-project)
      (define-key prefix-map (kbd "d") #'rubocop-check-directory)
      (define-key prefix-map (kbd "f") #'rubocop-check-current-file)
      (define-key prefix-map (kbd "P") #'rubocop-autocorrect-project)
      (define-key prefix-map (kbd "D") #'rubocop-autocorrect-directory)
      (define-key prefix-map (kbd "F") #'rubocop-autocorrect-current-file)
      (define-key prefix-map (kbd "X") #'rubocop-format-project)
      (define-key prefix-map (kbd "y") #'rubocop-format-directory)
      (define-key prefix-map (kbd "x") #'rubocop-format-current-file)

      (define-key map rubocop-keymap-prefix prefix-map))
    map)
  "Keymap for RuboCop mode.")

;;;###autoload
(define-minor-mode rubocop-mode
  "Minor mode to interface with RuboCop."
  :lighter " RuboCop"
  :keymap rubocop-mode-map
  :group 'rubocop
  (if rubocop-mode
      ;; on mode enable
      (progn
        (add-hook 'before-save-hook 'rubocop-autocorrect-current-file-silent nil t)
        (add-hook 'before-save-hook 'rubocop-format-current-file-silent nil t))
    ;; on mode disable
    (remove-hook 'before-save-hook 'rubocop-autocorrect-current-file-silent t)
    (remove-hook 'before-save-hook 'rubocop-format-current-file-silent t)))

(provide 'rubocop)

;;; rubocop.el ends here
