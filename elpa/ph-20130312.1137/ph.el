;; -*- lexical-binding: t -*-

(require 'ph-venture)

;; shut up the compiler
(defvar ph-buffer-pobj)
(defvar ph-buffer-orig-file-name)

;; Set it to a file name in ph-project-close/-open, then in
;; ph-find-file-hook/ph-kill-file-hook do (cl-return) only if
;; ph-status-busy == buffer-file-name.
(defvar ph-status-busy nil "A global semaphore for file hooks")

;; Used in unit tests. Increments only if did the job. May overflow, we
;; doesn't care.
(defvar ph-status-find-file-hook 0)
(defvar ph-status-kill-buffer-hook 0)
(defvar ph-status-dired-after-readin-hook 0)
(defvar ph-status-before-save-hook 0)



(defun ph-find-file-hook()
  (cl-block nil
	(let (db pobj file)
	  (if (or (not buffer-file-name)
			  (equal ph-status-busy buffer-file-name)
			  (not (setq db (ph-db-find buffer-file-name))))
		  (cl-return))

	  (when (setq pobj (ph-vl-find db))
		(setq file (ph-file-relative buffer-file-name (ph-dirname db)))

		(ph-buffer-pobj-set pobj)
		(ph-venture-opfl-add pobj file)
		(ph-venture-marshalling pobj)

		(cl-incf ph-status-find-file-hook))
	  )))

(defun ph-kill-buffer-hook()
  (cl-block nil
	(if (or (not buffer-file-name)
			(equal ph-status-busy buffer-file-name)
			(not (ph-buffer-pobj-get)))
		(cl-return))

	(let (pobj file)
	  (setq pobj (ph-buffer-pobj-get))
	  (setq file (ph-file-relative buffer-file-name
								   (ph-venture-opfl-prefix pobj)))

	  (ph-venture-opfl-rm pobj file)
	  (cl-incf ph-status-kill-buffer-hook)
	  (unless (ph-venture-marshalling pobj)
		;; restore file in ph-vl POBJ if marshalling failed
		(ph-venture-opfl-add pobj file))
	  )))

(defun ph-dired-after-readin-hook ()
  "Marks buffer as belonging to project if dired dir is a child to project dir.
This hook doesn't need ph-status-busy checks because it doesn't
write to db."
  (cl-block nil
	(let (pobj cwd db)
	  (unless (stringp dired-directory) (cl-return))
	  (setq cwd (directory-file-name (expand-file-name dired-directory)))

	  (unless (setq db (ph-db-find cwd)) (cl-return))

	  (when (setq pobj (ph-vl-find db))
		(ph-buffer-pobj-set pobj)
		(cl-incf ph-status-dired-after-readin-hook))
	  )))

(defun ph-before-save-hook ()
  "Simple protection from (set-visited-file-name).
This hook doesn't need ph-status-busy checks because, h'm, it's too
long to explain."
  (cl-block nil
	  (let (pobj)
		(if (or
			 (not buffer-file-name)
			 (not (setq pobj (ph-buffer-pobj-get))))
			(cl-return))

		(when (not
			   (string-prefix-p (ph-venture-opfl-prefix pobj) buffer-file-name))
;		  (print (format "OUT %s from %s"
;						 buffer-file-name (ph-venture-opfl-prefix pobj)))
		  ;; user has pointed the buffer to some file that is NOT in a
		  ;; project directory
		  (ph-venture-opfl-rm pobj ph-buffer-orig-file-name)
		  (ph-buffer-pobj-unset)

		  (cl-incf ph-status-before-save-hook)
		  (cl-return))

		(when (not (equal buffer-file-name
						  (ph-venture-opfl-absolute
						   pobj ph-buffer-orig-file-name)))
;		  (print (format "IN %s != %s"
;						 buffer-file-name (ph-venture-opfl-absolute
;										   pobj ph-buffer-orig-file-name)))

		  ;; buffer was moved in boundaries of the project directory
		  (ph-venture-opfl-rm pobj ph-buffer-orig-file-name)
		  (setq ph-buffer-orig-file-name (ph-file-relative
										  buffer-file-name
										  (ph-venture-opfl-prefix pobj)))
		  (ph-venture-opfl-add pobj ph-buffer-orig-file-name)
		  (ph-venture-marshalling pobj)

		  (cl-incf ph-status-before-save-hook)
		  )))
	)



(defun ph-buffer-pobj-get (&optional buf)
  (unless (bufferp buf) (setq buf (current-buffer)))

  (if (and (local-variable-p 'ph-buffer-pobj buf)
		   (ph-ven-p (buffer-local-value 'ph-buffer-pobj buf)))
	  (buffer-local-value 'ph-buffer-pobj buf)))

(defun ph-buffer-pobj-set (pobj)
  "Set in current buffer a local variable that 'marks' the buffer
as POBJ belonging. We make it permanent-local to survive through
major mode changes."
  (when (ph-ven-p pobj)
	(setq-local ph-buffer-pobj pobj)
	(put 'ph-buffer-pobj 'permanent-local t)

	(when buffer-file-name
	  (setq-local ph-buffer-orig-file-name
				  (ph-file-relative buffer-file-name (ph-venture-opfl-prefix pobj)))
	  (put 'ph-buffer-orig-file-name 'permanent-local t)
	  )))

(defun ph-buffer-pobj-unset ()
  (ignore-errors
	(kill-local-variable 'ph-buffer-pobj)
	(kill-local-variable 'ph-buffer-orig-file-name)
	))

(defun ph-buffer-list-pobj-set (pobj)
  "Iterate through (buffer-list) & mark all possible POBJ buffers
that weren't marked already. This is usefull only
in (ph-project-new) for a case when user opens some files in
'foo' dir at first & then creates a project in 'foo' dir.

Doesn't do any I/O.
Return a number of marked buffers."
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return 0))

	(let ((mBuffers 0)
		  (bufCount 0)
		  (report (make-progress-reporter
				   "Searching for project buffers... " 0 (length (buffer-list))))
		  fname)
	  (dolist (idx (buffer-list))
		(when (and (not (ph-buffer-pobj-get idx))
				   (setq fname (buffer-file-name idx))
				   (string-prefix-p (ph-venture-opfl-prefix pobj) fname))
		  (with-current-buffer idx
			(ph-buffer-pobj-set pobj))
		  (ph-venture-opfl-add pobj (ph-file-relative
									 fname (ph-venture-opfl-prefix pobj)))
		  (cl-incf mBuffers))

		(progress-reporter-update report (cl-incf bufCount)))

	  (progress-reporter-done report)
	  mBuffers
	  )))

(cl-defun ph-buffer-list (pobj &key nocb names)
  "Iterate through (buffer-list) & return only POBJ buffers.
NOCB meams don't include current buffer in the result;
NAMES means return just buffer names, not full buffer objects."
  (cl-block nil
	(let (buflist cell)
	  (unless (ph-ven-p pobj) (cl-return nil))

	  (dolist (idx (buffer-list))
		(catch 'continue
		  (if (and nocb (equal idx (current-buffer))) (throw 'continue nil))

		  (when (and (setq cell (ph-buffer-pobj-get idx)) (eq pobj cell))
			(if names
				(push (buffer-name idx) buflist)
			  (push idx buflist))
			)))

	  (reverse buflist))))



(defun ph-project-which (&optional pobj)
  "Print a path to a project db for current buffer.
Return pobj db or nil on error."
  (interactive)
  (cl-block nil
	(when (and (not (ph-ven-p pobj))
			   (not (setq pobj (ph-buffer-pobj-get))))
	  (ph-warn 1 "%s doesn't belong to any opened project" (current-buffer))
	  (cl-return nil))

	(ph-warn 0 "%s: %s" (ph-venture-name pobj) (ph-ven-db pobj))
	(ph-ven-db pobj)
	))

(defun ph-project-parse (file)
  "Load a project form FILE as db and return the project object.
If the project already loaded, just return a pointer to ph-vl list.
Doesn't load any opfl files.

Return nil on error."
  (cl-block nil
	(if (or (not file) (not (stringp file))) (cl-return nil))

	(let (pobj)
	  (if (setq pobj (ph-vl-find file)) (cl-return pobj))

	  (when (not (setq pobj (ph-venture-unmarshalling file)))
		(ph-warn 1 "cannot parse project %s" file)
		(cl-return nil))

	  (ph-vl-add pobj)
	  )))

;; 0) Parses FILE.
;; 1) Adds the project to ph-vl list.
;; 2) For each file in a project a) opens it, b) points buffer local
;;	  variable to the project object.
(defun ph-project-open (file)
  "Return a number of opened files or nil on error."
  (interactive "fOpen .ph file: ")
  (cl-block nil
	(unless file (cl-return nil))

	(let ((openedFiles 0) (nFile 0)
		  report pobj cell)
	  (when (not (setq pobj (ph-venture-unmarshalling file)))
		(ph-warn 1 "cannot parse project %s" file)
		(cl-return nil))
	  (when (ph-vl-find file)
		(ph-warn 1 "project %s is already loaded in emacs" file)
		(cl-return nil))

;	  (print (nth 5 (file-attributes file)))
	  (setq cell (ph-vl-add pobj))
	  (setq report (make-progress-reporter
					(format "Opening %s... " file) 0 (ph-venture-opfl-size pobj)))
	  (unwind-protect
		  (ph-venture-opfl-each pobj
								(lambda (key _val)
								  (setq ph-status-busy
										(ph-venture-opfl-absolute pobj key))
								  (if (file-readable-p ph-status-busy)
									  (condition-case err
										  (when (find-file ph-status-busy)
											(ph-buffer-pobj-set cell)
											(cl-incf openedFiles))
										(error
										 (ph-venture-opfl-rm pobj key)
										 (ph-warn 1 "find-file failed: %s"
												  (error-message-string err))))
									(ph-venture-opfl-rm pobj key))

								  (progress-reporter-update report (cl-incf nFile))
								  ))
		;; always make sure that hooks are working again
		(setq ph-status-busy nil)
		(progress-reporter-done report))

	  ;; mark already opened buffers
	  (ph-buffer-list-pobj-set pobj)
	  ;; sync db with memory objects
	  (ph-venture-marshalling pobj)

	  (ph-project-dired-open (ph-venture-name pobj))
	  openedFiles)))

(defun ph-project-close-by-db (db)
  "Use only in dynamic menu generation."
  (let ((pobj (ph-vl-find db)))
	(if pobj (ph-project-close pobj))))

(defun ph-project-close (&optional pobj)
  "Close all currently opened project files. Return t on success."
  (interactive)
  (cl-block nil
	(when (and (not (ph-ven-p pobj))
			   (not (setq pobj (ph-buffer-pobj-get))))
	  (ph-warn 1 "%s doesn't belong to any opened project" (current-buffer))
	  (cl-return nil))

	(let* ((buflist (ph-buffer-list pobj))
		   (report (make-progress-reporter
					(format "Closing %s... " (ph-ven-db pobj)) 0 (length buflist)))
		   (nFile 0))

	  ;; kill buffers in usual emacs fashion, some buffers may be unsaved
	  ;; & user can press C-g thus killing only a subset of buffers
	  (unwind-protect
		  (dolist (idx buflist)
			(setq ph-status-busy (buffer-file-name idx))
			(with-demoted-errors
			  (if idx (kill-buffer idx)))
			(progress-reporter-update report (cl-incf nFile)))
		;; always make sure that hooks are working again
		(setq ph-status-busy nil)
		(progress-reporter-done report)))

	;; remove project from ph-vl if user didn't hit C-g
	(ph-vl-rm (ph-ven-db pobj))
	t))

(defun ph-project-new (dir)
  "Create a new project in DIR. If DIR doens't exist it will be created.
Return a path to db.  If DIR is a subproject, close parent
project & clean its db from subproject files."
  (interactive "GCreate project in: ")
  (cl-block nil
	(unless dir (cl-return nil))
	(setq dir (expand-file-name dir))

	(let ((db (ph-db-get dir))
		  parDb parObj pobj)
	  (if (file-exists-p db)
		  (error "There is already a project in %s" dir))

	  (when (setq parDb (ph-db-find-subproject dir))
		(if (not (y-or-n-p (format "Directory %s is alredy under project %s. \
Make a sub-project?" dir parDb)))
			(cl-return nil)
		  ;; Close a sub project & fix its db.	Of cource it's better
		  ;; to "transfer" opfl subproject's files to a new project
		  ;; in real time, but that's too much work & emacs is an old fart.
		  (when (not (setq parObj (ph-project-parse parDb)))
			(error "Parsing sub-project %s failed. \
New project was NOT created" parDb))
		  (ph-project-close parObj)
		  (ph-venture-clean parObj (ph-file-relative dir (ph-dirname parDb)))
		  (when (not (ph-venture-marshalling parObj))
			(error "Updating sub-project %s failed. \
New project was NOT created" parDb))
		  ))
	  (if (not (file-directory-p dir)) (mkdir dir t))

	  (setq pobj (ph-venture-new db))
	  ;; mark already opened buffers
	  (ph-buffer-list-pobj-set pobj)
	  (unless (ph-venture-marshalling pobj)
		(error "Cannot create project in %s" dir))
	  ;; open dired & forcibly mark it as a project buffer
	  (if (find-file dir) (ph-buffer-pobj-set pobj))

	  db
	  )))

(defun ph-project-switch-buffer (&optional pobj)
  "Like ido-swithc-buffer but only for a specific POBJ.
Return a buffer name if switch was done."
  (interactive)
  (if (and (not (ph-ven-p pobj))
		   (not (setq pobj (ph-buffer-pobj-get))))
	  (error "%s doesn't belong to any opened project" (current-buffer)))

  (let (buflist buf)
	;; create a list of POBJ emacs buffer names (not file names)
	;; skipping current buffer
	(if (not (setq buflist (ph-buffer-list pobj :nocb t :names t)))
		(error "Project %s doesn't have opened files yet" (ph-ven-db pobj)))

	(if (= 0 (length buflist))
		(progn
		  (ph-warn 1 "%s is the only 1 opened in this project" (current-buffer))
		  (current-buffer))
	  (when (setq buf (ido-completing-read "ph: " buflist))
		(switch-to-buffer buf))
	  buf)
	))

(defun ph-project-dired-open (name)
  "Open a dired buffer with root directory of NAME project.
NAME is a string that only ph-venture-name function can return."
  (if name
	  (find-file (ph-venture-opfl-prefix (ph-vl-find-by-name name)))))

(defun ph-project-select ()
  "Ido-style project selection. Return selected project name.
Unlike ph-project-switch-buffer it doesn't consider previous user's choices."
  (if (= 0 (ph-vl-size))
	  (error "No opened projects yet. Type ph-project-open or ph-project-new"))

  (ido-completing-read "Project: " (ph-vl-names)))

(defun ph-project-switch ()
  "Switch to a root directory of a selected opened project.
Return selected project name."
  (interactive)
  (let (choice)
	(when (setq choice (ph-project-select))
	  (ph-project-dired-open choice))

	choice
	))

(defun ph-project-switch-buffer-other-project ()
  "Switch to project, then switch to some buffer. Very handy.
Return selected buffer."
  (interactive)
  (let (projName)
	(when (setq projName (ph-project-select))
	  (ph-project-switch-buffer (ph-vl-find-by-name projName)))
	))

(defun ph-project-file-mv (dest)
  "Move current buffer to DEST."
  (interactive "Gmv destination: ")
  (unless buffer-file-name
	(error "%s is not visiting a file" (buffer-name)))
  (unless (and dest (> (length dest) 0))
	(error "Invalid destination"))

  (setq dest (expand-file-name dest))
  (if (equal buffer-file-name dest)
	  (error "Source & destination are equal"))

  (let (needSave)
	;; force 'save' for project files only to auto-update ph-vl et
	;; al. in before-save-hook.
	(if (ph-buffer-pobj-get) (setq needSave t))

	;; user entered a directory as a destination
	(if (or (equal (file-name-as-directory dest) dest)
			(file-directory-p dest))
		(setq dest (concat (file-name-as-directory dest)
						   (file-name-nondirectory buffer-file-name))))
;	(ph-puts "%s %s" buffer-file-name dest)

	(mkdir (file-name-directory dest) t)
	(rename-file buffer-file-name dest t)
	(set-visited-file-name dest)
	(if needSave (basic-save-buffer))))



;;;###autoload
(define-minor-mode ph-mode
  "Toggle global minor Project Helper mode.
See https://github.com/gromnitsky/ph for the help.

\\{ph-mode-map}
\\[ph-project-open]      Open a .ph file.
\\[ph-project-close]     Close opened project files.
\\[ph-project-which]     Shows project name for current buffer.
\\[ph-project-new]       Create a new (sub)project in some directory.
\\[ph-project-switch]    Switch to a root of another project."
  :lighter (:eval (ph-modeline))
  :keymap '(([M-f3] . ph-project-switch-buffer)
			([s-f3] . ph-project-switch-buffer-other-project))
  :global t
  (if ph-mode
	  (progn
		(add-hook 'find-file-hook 'ph-find-file-hook)
		(add-hook 'dired-after-readin-hook 'ph-dired-after-readin-hook)
		(add-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
		(add-hook 'before-save-hook 'ph-before-save-hook))
	(remove-hook 'find-file-hook 'ph-find-file-hook)
	(remove-hook 'dired-after-readin-hook 'ph-dired-after-readin-hook)
	(remove-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	(remove-hook 'before-save-hook 'ph-before-save-hook)
	))

(defun ph-modeline ()
  (if (ph-buffer-pobj-get)
	  " ph"
	""))

(defun ph-menu-generate (_dummy)
  (cl-block nil
	(let (menu name db displayName)
	  ;; static portion
	  (setq menu
			'(["New" ph-project-new]
			  ["Open" ph-project-open]
			  ["Close Current" ph-project-close]
			  ["Show Current Name" ph-project-which]))

	  (if (= 0 (ph-vl-size)) (cl-return menu))

	  ;; dynamic portion
	  (setq menu (append menu '("----")))
	  (ph-vl-each (lambda (idx)
					(setq name (ph-venture-name idx))
					(setq displayName (format "%s (%d)" name
											  (ph-venture-opfl-size idx)))
					(setq db (ph-ven-db idx))
					(setq menu (append
								menu
								(list `(
										,displayName
										["Switch To"
										 (lambda ()
										   (interactive)
										   (ph-project-dired-open ,name))]
										["Close"
										 (lambda ()
										   (interactive)
										   (ph-project-close-by-db ,db))]
										))))
					))
;	  (print menu)
	  menu
	  )))

(easy-menu-define ph-menu-5705f1cee356eb1f ph-mode-map
  "Menu used when ph-mode minor mode is active."
  '("Ph" :filter ph-menu-generate))

(provide 'ph)
