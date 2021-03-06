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
  ;; boost is the secret of my energy
  (add-to-list 'achead:include-directories '"/usr/local/Cellar/boost/1.57.0/include/boost")
  (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc/4.9.2_1/include/c++/4.9.2")
  )
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(setq ac-show-menu-immediately-on-auto-complete t)

;; Yasnippet setup
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
;; (semantic-mode 1)
;; (defun my:add-semantics-to-autocomplete()
;;  (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'my:add-semantics-to-autocomplete)
;; (add-hook 'c++-mode-common-hook 'my:add-semantics-to-autocomplete)
;; (global-semantic-idle-scheduler-mode 1)

;; Setting up selection region key binds
(global-set-key (kbd "C-c =") 'er/expand-region)
(autoload 'er/contract-region "expand-region")

;; Git status
(global-set-key (kbd "C-c g s") 'magit-status)

;; Projectile setup
(require 'projectile)
(projectile-global-mode)
(global-set-key (kbd "C-c p s") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c p f") 'projectile-find-file-in-known-projects)
(global-set-key (kbd "C-c p k") 'projectile-kill-buffers)

;; ido - Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; I like 80-Column-view
;; (require 'column-marker)
;; (add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))
;; (add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
