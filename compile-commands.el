(defvar-local compile-commands-filename "compile_commands.json")

(defun compile-commands--parse-json ()
  (with-temp-buffer
    (let* ((project-dir (project-root (project-current t)))
           (file (expand-file-name compile-commands-filename project-dir)))
      (insert-file-contents file)
      (json-parse-buffer))))

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

(setq compile-commands--include-regexp " -I\\([[:alnum:]:.\\/_-]*\\)"
      compile-commands--include-system-regexp " -isystem \\([[:alnum:]:.\\/_-]*\\)"
      compile-commands--external-include-regexp " -external:I\\([[:alnum:]:.\\/_-]*\\)")

(defun compile-commands--parse-include-directories (command)
  (append (compile-commands--re-match compile-commands--include-regexp command)
          (compile-commands--re-match compile-commands--include-system-regexp command)
          (compile-commands--re-match compile-commands--external-include-regexp command)))

(defun compile-commands--keys (hash-table)
  (let ((ret nil))
    (maphash (lambda (k v) (push k ret)) hash-table)
    ret))

(defun compile-commands-get-include-directories ()
  (let ((table (make-hash-table :test 'equal :weakness nil)))
    (dolist (command (compile-commands--parse-commands))
      (dolist (include (compile-commands--parse-include-directories command))
        (puthash include nil table)))
    (compile-commands--keys table)))

(provide 'compile-commands)
