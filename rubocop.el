;;; rubocop.el --- An Emacs interface for RuboCop

;; Copyright © 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/rubocop-emacs
;; Version: 0.1
;; Keywords: project, convenience
;; Package-Requires: ((dash "1.0.0"))

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
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

(require 'dash)

(defgroup rubocop nil
  "An Emacs interface for RuboCop."
  :group 'tools
  :group 'convenience)

(defvar rubocop-project-root-files
  '(".projectile" ".git" ".hg" ".bzr" "_darcs" "Gemfile")
  "A list of files considered to mark the root of a project.")

(defun rubocop-project-root ()
  "Retrieve the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or (->> rubocop-project-root-files
        (--map (locate-dominating-file default-directory it))
        (-remove #'null)
        (car))
      (error "You're not into a project")))

(defun rubocop-buffer-name (file-or-dir)
  "Generate a name for the RuboCop buffer from FILE-OR-DIR."
  (concat "*RuboCop " file-or-dir "*"))

;;;###autoload
(defun rubocop-run-on-project ()
  "Run on current project."
  (interactive)
  (rubocop-run-on-directory (rubocop-project-root)))

;;;###autoload
(defun rubocop-run-on-directory (&optional directory)
  "Run on DIRECTORY if present.
Alternatively prompt user for directory."
  (interactive)
  (rubocop-ensure-installed)
  (let ((directory
         (or directory
             (read-directory-name "Select directory:"))))
    (compilation-start
     (concat "rubocop -es " directory)
     'compilation-mode
     (lambda (arg) (message arg) (rubocop-buffer-name directory)))))

;;;###autoload
(defun rubocop-run-on-current-file ()
  "Run on current file."
  (interactive)
  (rubocop-ensure-installed)
  (let ((file-name (buffer-file-name (current-buffer))))
    (if file-name
        (compilation-start
         (concat "rubocop -es " file-name)
         'compilation-mode
         (lambda (arg) (rubocop-buffer-name file-name)))
      (error "Buffer is not visiting a file"))))

(defun rubocop-ensure-installed ()
  "Check if RuboCop is installed."
  (unless (executable-find "rubocop")
    (error "RuboCop is not installed")))

(provide 'rubocop)

;;; rubocop.el ends here
