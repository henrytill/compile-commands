;;; compile-commands.el --- A library for working with JSON Compilation Database files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Henry Till

;; Author: Henry Till <henrytill@gmail.com>
;; Package-Requires: ((emacs "26.3"))
;; Keywords: c, convenience, languages
;; URL: https://git.sr.ht/~henrytill/compile-commands


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions for getting information from JSON Compilation Database Files.

;;; Code:

(require 'json)
(require 'project)

(defvar-local compile-commands-filename "compile_commands.json")

(defvar compile-commands--include-regexp          " -I\\([[:alnum:]:.\\/_-]*\\)")
(defvar compile-commands--include-system-regexp   " -isystem \\([[:alnum:]:.\\/_-]*\\)")
(defvar compile-commands--external-include-regexp " -external:I\\([[:alnum:]:.\\/_-]*\\)")

(defun compile-commands--parse-json ()
  (let* ((project-dir (when (fboundp 'project-root) (project-root (project-current t))))
         (file (expand-file-name compile-commands-filename project-dir))
         (json-object-type 'hash-table))
    (json-read-file file)))

(defun compile-commands--parse-commands ()
  (let ((json (compile-commands--parse-json))
        (f (lambda (x) (gethash "command" x ""))))
    (seq-map f json)))

(defun compile-commands--re-match (regexp string)
  (save-match-data
    (let ((case-fold-search nil)
          (pos 0)
          (ret nil))
      (while (string-match regexp string pos)
        (push (match-string 1 string) ret)
        (setq pos (match-end 1)))
      ret)))

(defun compile-commands--parse-include-directories (command)
  (append (compile-commands--re-match compile-commands--include-regexp command)
          (compile-commands--re-match compile-commands--include-system-regexp command)
          (compile-commands--re-match compile-commands--external-include-regexp command)))

(defun compile-commands--keys (hash-table)
  (let* ((ret nil)
         (f (lambda (k v) (push k ret))))
    (maphash f hash-table)
    ret))

(defun compile-commands-get-include-directories ()
  (let ((table (make-hash-table :test 'equal :weakness nil)))
    (dolist (command (compile-commands--parse-commands))
      (dolist (include (compile-commands--parse-include-directories command))
        (puthash include nil table)))
    (compile-commands--keys table)))

(provide 'compile-commands)
;;; compile-commands.el ends here
