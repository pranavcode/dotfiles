;;; git-draft.el --- draft git commit messages -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes
;; Version: 0.0.2
;; Package-requires: ((dash "2.9.0"))
;; Url: http://github.com/nicferrier/emacs-git-draft

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This let's you make a draft of what has changed in your git project
;; for inserting into commit messages.  In your commit buffer do:

;;   M-x git-draft

;; and git-draft will analyze your changes and try and pull out
;; function names that have changed.

;; It probably only works for EmacsLisp and similar languages.  And
;; it's just not very good anyway.  But it's a start.  Maybe.

;; == Obvious todos

;; We clearly need to be able to install into a hook for git commit,
;; either magit's or vc's.  I guess that should be configurable or
;; something.


;;; Code:

(require 'rx)
(require 's)
(require 'dash)

(defun git-draft/diff ()
  "Diff the git index and parse the result.

Returns an ordered (as found in the source diff) list of diff
elements which are p-lists beginning with either `:file' or
`:hunk'.

`:file' elements contain a `:from' indicating the from file and a
`:to' indicating the to file.

`:hunk' elements contain a `:from-start', `:from-count',
`:to-start' and an optional `:to-count' integer."
  (reverse
   (let (result)
     (with-temp-buffer
       (let ((pager (getenv "GIT_PAGER")))
         (setenv "GIT_PAGER" "")
         (unwind-protect
              (shell-command "git diff -U --cached ." (current-buffer))
           (when pager 
             (setenv "GIT_PAGER" pager))))
       (goto-char (point-min))
       (while (re-search-forward 
               (rx (or (and line-start ; diff start
                            (group-n 1 "diff --git ") 
                            "a/" (group-n 2 (1+ (any "a-zA-Z0-9._-"))) " "
                            "b/" (group-n 3 (1+ (any "a-zA-Z0-9._-")))
                            line-end)
                       (and line-start  ; hunk start
                            (group-n 1 "@@ -")
                            (group-n 2 (1+ (any "0-9")))
                            "," (group-n 3 (1+ (any "0-9")))
                            " +" (group-n 4 (1+ (any "0-9")))
                            (* (and "," (group-n 5 (1+ (any "0-9")))))
                            " @@"  (* not-newline) line-end)))
               nil t)
         (if (equal (match-string 1) "@@ -")
             (push (list :hunk
                         :from-start (string-to-int (match-string 2))
                         :from-count (string-to-int (match-string 3))
                         :to-start (string-to-int (match-string 4))
                         :to-count (string-to-int (match-string 5)))  result)
             ;; Else it's a diff start
             (push (list :file
                         :from (match-string 2)
                         :to (match-string 3)) result)))
       result))))

(defun git-draft/current-defun ()
  "Maybe an alternative to `add-log-current-defun'.

This is very simple, it just looks for things with a face
indicator of defn-ness."
  (beginning-of-defun)
  (while (not (memq (get-text-property (point) 'face)
                    '(font-lock-variable-name-face
                      font-lock-function-name-face)))
    (goto-char
     (next-single-property-change
      (point) 'face nil (line-end-position)))
    (when (= (point) (line-end-position))
      (error "no defun found")))
  (buffer-substring-no-properties
   (point)
   (next-single-property-change
    (point) 'face nil (line-end-position))))

(defun git-draft/make-draft ()
  "Make a draft git commit message."
  (let ((buf (current-buffer))
        result)
    (unwind-protect
         (let ((lst (git-draft/diff)))
           (dolist (e lst)
             (if (equal (car e) :file)
                 (set-buffer (find-file-noselect (plist-get (cdr e) :from)))
                 ;; Else it's a hunk
                 (save-excursion
                   (goto-line 
                    (+ (plist-get (cdr e) :from-start)
                       (or (plist-get (cdr e) :from-count) 0)))
                   (condition-case err
                       (push (git-draft/current-defun) result)
                     (error ; just ignore errors and move on
                      ))))))
      (set-buffer buf))
    (reverse result)))

;;;###autoload
(defun git-draft ()
  "Insert a draft commit message into the current-buffer.

Presumably the current buffer is a commit buffer."
  (interactive)
  (insert
   (s-join "\n" (--map (concat it ": ") (git-draft/make-draft)))))

;;; git-draft.el ends here
