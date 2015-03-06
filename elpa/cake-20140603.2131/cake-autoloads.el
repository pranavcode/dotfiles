;;; cake-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cake" "cake.el" (21716 53069 0 0))
;;; Generated autoloads from cake.el

(autoload 'cake "cake" "\
CakePHP minor mode.

\(fn &optional ARG)" t nil)

(when (fboundp 'define-global-minor-mode) (define-global-minor-mode global-cake cake cake-maybe :group 'cake))

(autoload 'cake-snippets-initialize "cake" "\


\(fn)" nil nil)

(eval-after-load 'yasnippet '(cake-snippets-initialize))

;;;***

;;;### (autoloads nil nil ("cake-pkg.el") (21716 53069 27737 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cake-autoloads.el ends here
