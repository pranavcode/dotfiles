;; -*- lexical-binding: t -*-

(require 'json)

(defvar ph-meta (json-read-file
				 (concat (file-name-directory load-file-name) "/meta.json")))

(defconst ph-meta-version (cdr (assoc 'version ph-meta)))
(defconst ph-meta-name (cdr (assoc 'name ph-meta)))

(provide 'ph-metadata)
