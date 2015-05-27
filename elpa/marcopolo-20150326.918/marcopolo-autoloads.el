;;; marcopolo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "marcopolo-mode" "marcopolo-mode.el" (21852
;;;;;;  49221 0 0))
;;; Generated autoloads from marcopolo-mode.el

(autoload 'marcopolo-registry-search "marcopolo-mode" "\
Search from Docker registry repositories using `TERM' request.

\(fn TERM)" t nil)

(autoload 'marcopolo-hub-search "marcopolo-mode" "\
Search from Docker Hub repositories using `TERM' request.

\(fn TERM)" t nil)

;;;***

;;;### (autoloads nil "marcopolo-version" "marcopolo-version.el"
;;;;;;  (21852 49221 0 0))
;;; Generated autoloads from marcopolo-version.el

(autoload 'marcopolo-version "marcopolo-version" "\
Get the marcopolo version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

;;;***

;;;### (autoloads nil nil ("marcopolo-api.el" "marcopolo-custom.el"
;;;;;;  "marcopolo-hub.el" "marcopolo-pkg.el" "marcopolo-registry.el"
;;;;;;  "marcopolo-ui.el" "marcopolo-utils.el" "marcopolo.el") (21852
;;;;;;  49221 935111 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; marcopolo-autoloads.el ends here
