;;; icomplete+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "icomplete+" "icomplete+.el" (21754 8673 0
;;;;;;  0))
;;; Generated autoloads from icomplete+.el

(let ((loads (get 'Icomplete-Plus 'custom-loads))) (if (member '"icomplete+" loads) nil (put 'Icomplete-Plus 'custom-loads (cons '"icomplete+" loads))))

(defvar icompletep-exact-separator (if (> emacs-major-version 22) (string 9733 32) "* ") "\
String used by to separate exact match from other alternatives.")

(custom-autoload 'icompletep-exact-separator "icomplete+" t)

(defface icompletep-choices '((((background dark)) (:foreground "Snow4")) (t (:foreground "DarkBlue"))) "\
*Face for minibuffer reminder of possible completion suffixes." :group (quote Icomplete-Plus))

(defface icompletep-determined '((t (:foreground "SeaGreen"))) "\
*Face for minibuffer reminder of possible completion prefix." :group (quote Icomplete-Plus))

(defface icompletep-nb-candidates '((((background dark)) (:foreground "SpringGreen")) (t (:foreground "DarkMagenta"))) "\
*Face for minibuffer reminder of number of completion candidates.
This has no effect unless library `icicles.el' is being used." :group (quote Icomplete-Plus))

(defface icompletep-keys '((t (:foreground "Red"))) "\
*Face for minibuffer reminder of possible completion key bindings." :group (quote Icomplete-Plus))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; icomplete+-autoloads.el ends here
