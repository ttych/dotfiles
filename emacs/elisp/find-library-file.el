;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-library-file.el --- Functions and bindings to make library files
;;; more easy to examine and change.

(defun find-library-file (library)
  "Takes a single argument LIBRARY, being a library file to search for.
Searches for LIBRARY directly (in case relative to current directory,
or absolute) and then searches directories in load-path in order.  It
will test LIBRARY with no added extension, then with .el, and finally
with .elc.  If a file is found in the search, it is visited.  If none
is found, an error is signaled.  Note that order of extension searching
is reversed from that of the load function."
  (interactive "sFind library file: ")
  (let ((path (cons "" load-path)) exact match elc test found)
    (while (and (not match) path)
      (setq test (concat (car path) "/" library)
            match (if (condition-case nil
                          (file-readable-p test)
                        (error nil))
                      test)
            path (cdr path)))
    (setq path (cons "" load-path))
    (or match
        (while (and (not elc) path)
          (setq test (concat (car path) "/" library ".elc")
                elc (if (condition-case nil
                            (file-readable-p test)
                          (error nil))
                        test)
                path (cdr path))))
    (setq path (cons "" load-path))
    (while (and (not match) path)
      (setq test (concat (car path) "/" library ".el")
            match (if (condition-case nil
                          (file-readable-p test)
                        (error nil))
                      test)
            path (cdr path)))
    (setq found (or match elc))
    (if found
        (progn
          (find-file found)
          (and match elc
               (message "(library file %s exists)" elc)
               (sit-for 1))
          (message "Found library file %s" found))
      (error "Library file \"%s\" not found." library))))

(define-key ctl-x-4-map "l" 'find-library-file)
(define-key ctl-x-4-map "r" 'byte-recompile-directory)
