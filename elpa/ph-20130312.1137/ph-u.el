;; -*- lexical-binding: t -*-

(require 'cl-lib)

(require 'ph-metadata)

(defvar ph-verbose 1 "Verbosity level")



(defun ph-warn (level str &rest args)
  "Print a message via (message) according to LEVEL."
  (when (<= level ph-verbose)
	(if (/= 0 level) (setq str (concat ph-meta-name ": " str)))
	(message (apply 'format str args))
	))

(defun ph-puts (str &rest args)
  (print (apply 'format str args)))

(defun ph-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (if (not str) ""
	(while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
	  (setq str (replace-match "" t t str)))
	str))

(defun ph-dirname (d)
  "A robust dirname(1). Return '.' on error."
  (cl-block nil
	(if (not d) (cl-return "."))
	(setq d (ph-chomp d))
	(if (equal "" d) (cl-return "."))

	(let ((tmp (file-name-directory (directory-file-name d))))
	  (if (not tmp) (cl-return ".")
		(directory-file-name tmp))
	  )))

(defun ph-file-relative (file dir)
  "Return FILE name transformed as relative to DIR.
Raise error if DIR isn't a substring of FILE."
  (cl-block nil
	(unless file (error "FILE is nil"))
	(unless dir (cl-return file))

	(setq file (ph-chomp file))
	(setq dir (ph-chomp dir))
	(if (or (equal "" dir) (equal "." dir)) (cl-return file))

	(unless (string-prefix-p dir file)
	  (error "%s must be a prefix of %s" dir file))

	(let ((path (substring file (length dir))))
	  ;; remove possible leading '/' in the result
	  (if (and (> (length path) 0) (equal "/" (substring path 0 1)))
		  (substring path 1)
		path)
	  )))

(defun ph-file-read (file)
  "Read the contents of a file and return as a string.
Return nil on error."
  (cl-block nil
	(if (or (not file) (not (file-readable-p file))) (cl-return nil))

	(with-temp-buffer
	  (insert-file-contents file)
	  (buffer-string))))

(defun ph-buffer-mode-get (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
	major-mode))



(provide 'ph-u)
