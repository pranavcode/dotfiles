;;; bufshow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "bufshow" "bufshow.el" (21716 52973 0 0))
;;; Generated autoloads from bufshow.el

(defvar bufshow-mode nil "\
Non-nil if Bufshow mode is enabled.
See the command `bufshow-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bufshow-mode'.")

(custom-autoload 'bufshow-mode "bufshow" nil)

(autoload 'bufshow-mode "bufshow" "\
Bufshow mode is a presentation tool for Emacs.  Enabling the
`bufshow-mode' global minor mode is the first step to using it.
You'll also need to define an elisp vector that contains the list
of files and tokens to use during the presentation and invoke
`bufshow-load' or `bufshow-start' to start the presentation.

There are key bindings to move to the next and previous slides.
With an Emacs daemon and emacsclient it's easy to invoke the
`bufshow-next' and `bufshow-prev' functions using an IR remote
and something like lirc.

For more information on how to configure a presentation see the
`bufshow-start' function documentation.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bufshow-autoloads.el ends here
