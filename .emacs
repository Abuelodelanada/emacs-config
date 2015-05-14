;; Packages repository

(add-to-list 'load-path "~/.emacs.d/libraries")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Global settings

(setq inhibit-startup-message t) ;; Disable startup messages
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize window at startup
(setq make-backup-files nil) ;; No backup files ~
;; (setq backup-directory-alist `(("." . "~/.emacs-backup-files"))) ;; Backup files directory
(setq auto-save-default nil) ;; Stop creating auto #autosave# files
;; (setq multi-term-program "/bin/zsh") ;; Set default shell

;; Modes

(global-linum-mode 1) ;; Show line-number
(setq linum-format "%d ") ;; Separating line numbers from text
(tool-bar-mode -1) ;; Hide toolbar
(scroll-bar-mode -1) ;; Hide scrollbar
;; (menu-bar-mode -1) ;; Hide menubar
(column-number-mode 1) ;; Show column number
(global-auto-complete-mode t) ;; Enable auto-complete
(global-highlight-parentheses-mode t)

;; (load-file "~/.emacs.d/emacs-for-python/epy-init.el") ;; Python Emacs collection

;; Parse these extensions as PHP
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . php-mode))

;; Workgroups

(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)
;; (wg-load "~/.emacs.d/workgroups/group2")
;; (shell "*shell*") ;; Run shell

;; Dired customization

(require 'dired-sort-map)
(setq dired-listing-switches "--group-directories-first -alh")

(require 'dired-x)
(setq dired-omit-files "^\\...+$")

;; (put 'dired-find-alternate-file 'disabled nil) ;; Reuse directory buffer

;; Hooks

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(add-hook 'emacs-startup-hook (lambda () (multi-term)))

;; Themes

(load-file "~/.emacs.d/themes/atom-dark-theme.el")
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-lawrence)
;; (color-theme-charcoal-black)
;; (color-theme-renegade)

;; Custom keyboard shortcuts

(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window) 
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c d") 'duplicate-current-line)
(global-set-key (kbd "C-c C-z") 'term-stop-subjob)
(global-set-key (kbd "C-c C-w") 'copy-word)

;; Alias

(defalias 'bkr 'browse-kill-ring)
(defalias 'lb 'list-buffers)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

;; Functions

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )
     
(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )
     
(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
	       (progn (comint-next-prompt 25535) (yank))
	     (progn (goto-char (mark)) (yank) )))))
    (if arg
	(if (= arg 1)
	    nil
	  (funcall pasteMe))
      (funcall pasteMe))
    ))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  (message "Copying word at point into kill-ring...")
  ;; (Paste-to-mark arg)
  )

(defun duplicate-current-line ()
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

;; Disable linum for certain modes
(setq linum-mode-inhibit-modes-list '(shell-mode eshell-mode term-mode multi-term dired-mode doc-view-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))
(ad-activate 'linum-on)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
