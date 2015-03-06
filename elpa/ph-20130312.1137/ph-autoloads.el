;;; ph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ph" "ph.el" (21753 63578 0 0))
;;; Generated autoloads from ph.el

(defvar ph-mode nil "\
Non-nil if Ph mode is enabled.
See the command `ph-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ph-mode'.")

(custom-autoload 'ph-mode "ph" nil)

(autoload 'ph-mode "ph" "\
Toggle global minor Project Helper mode.
See https://github.com/gromnitsky/ph for the help.

\\{ph-mode-map}
\\[ph-project-open]      Open a .ph file.
\\[ph-project-close]     Close opened project files.
\\[ph-project-which]     Shows project name for current buffer.
\\[ph-project-new]       Create a new (sub)project in some directory.
\\[ph-project-switch]    Switch to a root of another project.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ph-metadata.el" "ph-pkg.el" "ph-u.el"
;;;;;;  "ph-venture.el") (21753 63578 588404 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ph-autoloads.el ends here
