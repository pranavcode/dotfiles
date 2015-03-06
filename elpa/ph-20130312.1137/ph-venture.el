;; -*- lexical-binding: t -*-

(require 'ph-u)

(defconst ph-SIM-DIR-MAX 64
  "Max number of projects in similar named directories, e.g. /a/foo,
/a/b/foo, c/foo")
(defconst ph-DB-NAME ".ph" "A physicall db file name")
(defvar ph-vl '() "Global list of currently opened projects")

;; See doc/structure.org for desc.
(cl-defstruct ph-ven
  "Project Helper object"
  db
  (version ph-meta-version)
  (opfl (make-hash-table :test 'equal))
  )



(defun ph-venture-name-simple (pobj)
  "Extract project name from POBJ. Return nil on error."
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return nil))

	(let ((name (file-name-nondirectory (ph-venture-opfl-prefix pobj))))
	  (if (equal "" name)
		  "Root"
		name))))

(defun ph-venture-name (pobj)
  "Extract project name from POBJ. Return nil on error.
Use ph-vl list to generate current unique name."
  (cl-block here
	(if (or (not (ph-ven-p pobj))
			(= 0 (ph-vl-size))) (cl-return-from here nil))

	;; projects: db => uniq_name
	;; table: uniq_name => 1
	(let ((projects (make-hash-table :test 'equal))
		  (table (make-hash-table :test 'equal))
		  result)

	  (ph-vl-each-rev (lambda (idx)
						(let ((name (ph-venture-name-simple idx))
							  nameN)

						  (if (not (gethash name table))
							  (progn
								(puthash name 1 table)
								(puthash (ph-ven-db idx) name projects))
							;; we have a project with similar name, try to
							;; check it as "name<N>", where N >= 2.
							(cl-loop for i from 2 to ph-SIM-DIR-MAX do
									 (setq nameN (format "%s<%d>" name i))

									 (when (not (gethash nameN table))
									   (puthash nameN 1 table)
									   (puthash (ph-ven-db idx) nameN projects)
									   (cl-return)))
							))))

	  (unless (setq result (gethash (ph-ven-db pobj) projects))
		(error "Probably ph-SIM-DIR-MAX was reached out--close some projects"))

	  result
	  )))

(defun ph-venture-opfl-add (pobj file)
  (if (equal (substring file 0 1) "/")
	  (error "%s: only relative paths allowed" file))
  (puthash file (float-time) (ph-ven-opfl pobj)))

(defun ph-venture-opfl-prefix (pobj)
  "Return a prefix useful for reconstructing absolute paths for opfl file names."
  (ph-dirname (ph-ven-db pobj)))

(defun ph-venture-opfl-absolute (pobj relName)
  "Return an absolute file name for RELNAME."
  (concat (ph-venture-opfl-prefix pobj) "/" relName))

(defun ph-venture-opfl-rm (pobj file)
  (remhash file (ph-ven-opfl pobj)))

(defun ph-venture-opfl-get (pobj file)
  (gethash file (ph-ven-opfl pobj)))

(defun ph-venture-opfl-size (pobj)
  (hash-table-count (ph-ven-opfl pobj)))

(defun ph-venture-opfl-each (pobj blk)
  "Iterate through POBJ opfl hash"
  (maphash (lambda (key val)
			 (funcall blk key val))
		   (ph-ven-opfl pobj)))

(defun ph-venture-new (file)
  "Create a new ph-ven with FILE as db & add it to ph-vl list.
Return a pointer to a cell in ph-vl list or nil on error.

Doesn't do any I/O."
  (cl-block nil
	(if (or (not file) (not (stringp file))) (cl-return nil))

	(ph-vl-add (make-ph-ven :db (expand-file-name file)))))

(defun ph-venture-marshalling (pobj)
  "Update POBJ in a marshalled form. Return t on success, nil otherwise.
WARNING: it rewrites the file every time."
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return nil))

	(condition-case nil
		;; most typical error would be "permission denied" if user has
		;; opened a project on a read-only partition
		(with-temp-file (ph-ven-db pobj)
		  (insert (prin1-to-string pobj)))
	  (error (cl-return nil)))
	t
	))

(defun ph-venture-unmarshalling (file)
  "Return parsed ph-ven object from FILE or nil on error."
  (cl-block nil
	(let ((raw (ph-file-read file))
		  pobj)
	  (condition-case nil
		  (setq pobj (read raw))
		(error (cl-return nil)))
	  (unless (ph-ven-p pobj) (cl-return nil))

	  ;; fix db location to current absolute value
	  (setf (ph-ven-db pobj) (ph-db-get (ph-dirname (expand-file-name file))))

	  pobj
	  )))

(defun ph-venture-clean (pobj dir)
  "Remove from POBJ any files with DIR prefix.
Return nil on error or list of removed files."
  (cl-block nil
	(if (or (not pobj) (not dir)) (cl-return nil))

	(let ((waste '()))
	  (ph-venture-opfl-each pobj (lambda (key _val)
								   (when (string-prefix-p dir key)
									 (push key waste)
									 )))

	  (cl-loop for idx in waste do (ph-venture-opfl-rm pobj idx))
	  waste
	  )))



(defun ph-vl-reset ()
  "Empty ph-vl list."
  (setq ph-vl '()))

(defun ph-vl-size ()
  "Self explanatory."
  (length ph-vl))

(defun ph-vl-each (blk)
  "Iterate through ph-vl list"
  (cl-loop for idx in ph-vl do (funcall blk idx)))

(defun ph-vl-each-rev (blk)
  "Iterate through ph-vl list"
  (cl-loop for idx in (reverse ph-vl) do (funcall blk idx)))

(defun ph-vl-find (db)
  "Return a pointer to some ph-vl object or nil."
  (cl-block nil
	(unless db (cl-return nil))
	(setq db (expand-file-name db))
	(ph-vl-each (lambda (pobj)
				  (if (equal db (ph-ven-db pobj)) (cl-return pobj))
				  ))
	nil
	))

(defun ph-vl-find-by-name (name)
  "Return a pointer to some ph-vl object or nil."
  (cl-block nil
	(unless name (cl-return nil))
	(ph-vl-each (lambda (idx)
				  (if (equal name (ph-venture-name idx)) (cl-return idx))
				  ))
	nil
	))

(defun ph-vl-rm (db)
  "Remove a ph-vl object from ph-vl list which db is DB.
Return t is something was removed, nil otherwise."
  (cl-block nil
	(let ((pobj (ph-vl-find db)))
	  (unless pobj (cl-return nil))
	  (setq ph-vl (delq pobj ph-vl))	; lisp is boring
	  t
	  )))

(defun ph-vl-add (pobj)
  "Add POBJ to ph-vl list.
Return a pointer to a cell in ph-vl list or nil on error."
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return nil))

	(let (cell)
	  (when (setq cell (ph-vl-find (ph-ven-db pobj)))
		(ph-warn 1 "project %s is already loaded in emacs" (ph-ven-db pobj))
		(cl-return cell))

	  (car (push pobj ph-vl)))))

(defun ph-vl-names ()
  "Return a list of currently opened project names or nil"
  (let ((names '()))
	(ph-vl-each (lambda (idx)
				  (push (ph-venture-name idx) names)))
	names
	))



(defun ph-db-get (dir)
  "Construct a proper db name from DIR."
  (if (not dir)
	  nil
	(concat (file-name-as-directory dir) ph-DB-NAME)))

(defun ph-db-find (file &optional startDir)
  "Return a project db file name or nil if FILE doesn't belong to any."
  (cl-block nil
	(setq file (ph-chomp file))

	(unless startDir (setq startDir file))
	(let ((db (concat startDir "/" ph-DB-NAME)))
	  (if (file-readable-p db)
		  (cl-return db))

	  ;; didn't find db file
	  (if (or (equal "." startDir) (equal "/" startDir)) (cl-return nil))

	  ;; Recursion!
	  (ph-db-find file (ph-dirname startDir))
	  )))

(defun ph-db-find-subproject (dir)
  "Return a subproject db file or nil if DIR doesn't belong to any.
If DIR is a file, detection will not work."
  (cl-block nil
	(let (spdb)
	  (unless dir (cl-return nil))
	  (if (and (setq spdb (ph-db-find (ph-dirname dir)))
			   (not (equal (expand-file-name dir) default-directory)))
		  spdb
		nil)
	  )))



(provide 'ph-venture)
