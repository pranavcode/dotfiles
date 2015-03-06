;;; cake2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cake-auto-switch" "cake-auto-switch.el" (21716
;;;;;;  53062 0 0))
;;; Generated autoloads from cake-auto-switch.el

(autoload 'cake-auto-switch "cake-auto-switch" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "cake2" "cake2.el" (21716 53062 0 0))
;;; Generated autoloads from cake2.el

(autoload 'cake2 "cake2" "\
CakePHP2 minor mode.

\(fn &optional ARG)" t nil)

(when (fboundp 'define-global-minor-mode) (define-global-minor-mode global-cake2 cake2 cake2::maybe :group 'cake2))

(defvar cake2::key-map (make-sparse-keymap) "\
Keymap for Cake2.")

(autoload 'cake2::snippets-initialize "cake2" "\


\(fn)" nil nil)

(eval-after-load 'yasnippet '(cake2::snippets-initialize))

;;;***

;;;### (autoloads nil nil ("cake2-pkg.el") (21716 53062 910177 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cake2-autoloads.el ends here
