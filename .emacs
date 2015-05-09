;; Packages repository

(add-to-list 'load-path "~/.emacs.d/libraries")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Global configurations

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
;; (put 'dired-find-alternate-file 'disabled nil) ;; Reuse directory buffer

;; Hooks

(add-hook 'emacs-startup-hook
  (lambda ()
    ;; (kill-buffer "*scratch*")
    (multi-term)
  ))

;; Themes

(load-file "~/.emacs.d/themes/atom-dark-theme.el")
;;(require 'color-theme)
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

;; Custom functions

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
(setq linum-mode-inhibit-modes-list '(shell-mode eshell-mode term-mode multi-term dired-mode))
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
