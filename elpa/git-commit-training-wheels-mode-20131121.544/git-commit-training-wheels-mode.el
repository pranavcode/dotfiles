;;; git-commit-training-wheels-mode.el --- Helps you craft well formed commit messages with git-commit-mode

;; Copyright (C) 2013 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 20131121.544
;; X-Original-Version: DEV
;; URL: https://github.com/yasuyk/git-commit-training-wheels-mode
;; Package-Requires: ((git-commit-mode "0.14.0"))
;; Keywords: convenience vc git

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

;; Helps you craft well formed commit messages with git-commit-mode
;; Directives for what makes a well formed commit come from
;; tpope: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

;; Based in part on https://github.com/re5et/magit-commit-training-wheels

;; Usage:
;;
;;   (require 'git-commit-training-wheels-mode) ;; Not necessary if using ELPA package
;;   (add-hook 'git-commit-mode-hook 'git-commit-training-wheels-mode)

;;; Code:

(require 'git-commit-mode)

(defadvice git-commit-commit (around git-commit-training-wheels disable)
  "Make sure we have a nice commit message."
  (let ((ok-to-commit t)
        (commit-problems nil)
        (case-fold-search nil))
    (save-excursion
      (beginning-of-buffer)
      (when (and (looking-at "[a-z]")
                 (yes-or-no-p
                  "First line doesn't start with a capital letter.  Fix? "))
        (capitalize-word 1))
      (end-of-line)
      (when (> (current-column) 50)
        (add-to-list 'commit-problems
                     "First line is too long (> 50 characters)."))
      (when (> (count-lines (point) (point-max)) 0)
        (forward-line)
        (when (and (not (equal (point-at-bol) (point-at-eol)))
                   (yes-or-no-p
                    "Doesn't have a blank line after the first.  Fix? "))
          (newline))
        (while (not (equal (point) (point-max)))
          (forward-line)
          (unless (looking-at "^#") ;; skip comment lines
            (end-of-line)
            (when (> (current-column) 72)
              (add-to-list
               'commit-problems
               "There are lines that are too long (> 72 characters)")))))
      (when commit-problems
        (catch 'break
          (dolist (problem commit-problems)
            (unless (yes-or-no-p (concat problem "  Commit anyway? "))
              (setq ok-to-commit nil)
              (throw 'break nil))))))
    (if ok-to-commit
        ad-do-it
      (git-commit-abort))))

;;;###autoload
(define-minor-mode git-commit-training-wheels-mode
  "Helps you craft well formed commit messages with `git-commit-mode'."
  :lighter    " GCTW"
  :init-value nil
  :global     nil
  (or (derived-mode-p 'git-commit-mode)
      (error "This mode only makes sense with `git-commit-mode'"))
  (cond (git-commit-training-wheels-mode
         (ad-enable-advice 'git-commit-commit 'around
                           'git-commit-training-wheels)
         (ad-activate 'git-commit-commit))
        (t
         (ad-disable-advice 'git-commit-commit 'around
                            'git-commit-training-wheels))))


(provide 'git-commit-training-wheels-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; git-commit-training-wheels-mode.el ends here
