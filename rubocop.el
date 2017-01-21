;;; rubocop.el --- An Emacs interface for RuboCop -*- lexical-binding: t -*-

;; Copyright © 2011-2016 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/rubocop-emacs
;; Version: 0.4.0
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
;; about stylistic issues in Ruby code.
;;
;;; Code:

(require 'tramp)

(defgroup rubocop nil
  "An Emacs interface for RuboCop."
  :group 'tools
  :group 'convenience)

(defvar rubocop-project-root-files
  '(".projectile" ".git" ".hg" ".bzr" "_darcs" "Gemfile")
  "A list of files considered to mark the root of a project.")

(defvar rubocop-check-command
  "rubocop --format emacs"
  "The command used to run RuboCop checks.")

(defvar rubocop-autocorrect-command
  "rubocop -a --format emacs"
  "The command used to run RuboCop's autocorrection.")

(defcustom rubocop-keymap-prefix (kbd "C-c C-r")
  "RuboCop keymap prefix."
  :group 'rubocop
  :type 'string)

(defun rubocop-local-file-name (file-name)
  "Retrieve local filename if FILE-NAME is opened via TRAMP."
  (cond ((tramp-tramp-file-p file-name)
         (tramp-file-name-localname (tramp-dissect-file-name file-name)))
        (t
         file-name)))

(defun rubocop-project-root ()
  "Retrieve the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or
   (car
    (mapcar #'expand-file-name
            (delq nil
                  (mapcar
                   (lambda (f) (locate-dominating-file default-directory f))
                   rubocop-project-root-files))))
   (error "You're not into a project")))

(defun rubocop-buffer-name (file-or-dir)
  "Generate a name for the RuboCop buffer from FILE-OR-DIR."
  (concat "*RuboCop " file-or-dir "*"))

(defun rubocop-build-command (command path)
  "Build the full command to be run based on COMMAND and PATH.
The command will be prefixed with `bundle exec` if RuboCop is bundled."
  (concat (if (rubocop-bundled-p) "bundle exec " "") command " " path))

(defun rubocop--dir-command (command &optional directory)
  "Run COMMAND on DIRECTORY (if present).
Alternatively prompt user for directory."
  (rubocop-ensure-installed)
  (let ((directory
         (or directory
             (read-directory-name "Select directory:"))))
    (compilation-start
     (rubocop-build-command command (rubocop-local-file-name directory))
     'compilation-mode
     (lambda (arg) (message arg) (rubocop-buffer-name directory)))))

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

(defun rubocop--file-command (command)
  "Run COMMAND on currently visited file."
  (rubocop-ensure-installed)
  (let ((file-name (buffer-file-name (current-buffer))))
    (if file-name
        (compilation-start
         (rubocop-build-command command (rubocop-local-file-name file-name))
         'compilation-mode
         (lambda (_arg) (rubocop-buffer-name file-name)))
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

(defun rubocop-bundled-p ()
  "Check if rubocop has been bundled."
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

      (define-key map rubocop-keymap-prefix prefix-map))
    map)
  "Keymap for RuboCop mode.")

;;;###autoload
(define-minor-mode rubocop-mode
  "Minor mode to interface with RuboCop."
  :lighter " RuboCop"
  :keymap rubocop-mode-map
  :group 'rubocop)

(provide 'rubocop)

;;; rubocop.el ends here
