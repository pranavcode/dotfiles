;; Some key binding
(global-set-key (kbd "C-c k") 'kill-whole-line)

;; Swap meta and super
(if (boundp 'mac-command-modifier) (setq mac-command-modifier 'meta))
(if (boundp 'mac-option-modifier) (setq mac-option-modifier 'super))

;; Make system and user specific emacs files
(setq eshell-history-file-name (concat (getenv "HOME") "/.emacs.d/eshell/" system-name "-history"))

;; Completion for minibuff
(icomplete-mode 99)
 
;; Add Melpa package archive
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa"."http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

;; Set default font size to 13pts
(set-face-attribute 'default nil :height 130)

;; Parens are detected while editing
(show-paren-mode)

;; Auto save options
(setq auto-save-interval 1)
(setq auto-save-timeout 1)

;; Enable column number, along side line number
(column-number-mode 1)

;; Disable tool bar 
(when (display-graphic-p)
   (tool-bar-mode 0))

;; Enable line numbers as left side bar
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")

;; Disable menu bar
(menu-bar-mode 0)

;; Save old emacs sessions
;; (desktop-save-mode 1)
;; (setq history-length 250)
;; (add-to-list 'desktop-globals-to-save 'file-name-history)

;; Auto complete config
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; for Linux, C++ 4.8, Boost 1.56.0
  (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu/c++/4.8")
  (add-to-list 'achead:include-directories '"/usr/include/c++/4.8")
  (add-to-list 'achead:include-directories '"/usr/include/c++/4.8.2")
  (add-to-list 'achead:include-directories '"/usr/include/boost_1_56_0/boost/")
  ;; for Darwin
  )
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Edit identical string occurances at same time
(require 'iedit)
(defun quit-iedit-mode ()
  "Turn off iedit-mode."
  (interactive)
  (iedit-mode -1))
(define-key global-map (kbd "C-c ;") 'iedit-mode) ;; start iedit mode
(define-key iedit-mode-keymap (kbd "RET") 'quit-iedit-mode) ;; stop iedit mode

;; Google C/C++ style checker
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

;; Theme config
(custom-set-variables
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Multi cursor config
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-all-like-this)

;; Turn on semantic mode
(semantic-mode 1)
(defun my:add-semantics-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantics-to-autocomplete)
(add-hook 'c++-mode-common-hook 'my:add-semantics-to-autocomplete)
(global-semantic-idle-scheduler-mode 1)

;; Setting up selection region key binds
(global-set-key (kbd "C-c r =") 'er/expand-region)
(autoload 'er/contract-region "expand-region")
(global-set-key (kbd "C-c r -") 'er/contract-region)
(autoload 'er/mark-inside-quotes "expand-region")
(global-set-key (kbd "C-c r i \"") 'er/mark-inside-quotes)
(global-set-key (kbd "C-c r i \'") 'er/mark-inside-quotes)
(autoload 'er/mark-inside-pairs "expand-region")
(global-set-key (kbd "C-c r i p") 'er/mark-inside-pairs)
(autoload 'er/mark-outside-quotes "expand-region")
(global-set-key (kbd "C-c r a \"") 'er/mark-outside-quotes)
(global-set-key (kbd "C-c r a \'") 'er/mark-outside-quotes)
(autoload 'er/mark-outside-pairs "expand-region")
(global-set-key (kbd "C-c r a p") 'er/mark-outside-pairs)
(autoload 'er/mark-inner-tag "expand-region")
(global-set-key (kbd "C-c r i t") 'er/mark-inner-tag)
(autoload 'er/mark-outer-tag "expand-region")
(global-set-key (kbd "C-c r a t") 'er/mark-outer-tag)

;; Git status
(global-set-key (kbd "C-c g s") 'magit-status)

(message "Welcome to Emacs. You are set!")
