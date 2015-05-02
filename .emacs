;; MELPA config
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Load python for emacs collection
;; (load-file "~/.emacs.d/emacs-for-python/epy-init.el")

(setq make-backup-files nil) ;; No backup files ~
;; (setq backup-directory-alist `(("." . "~/.emacs-backup-files"))) ;; Copy backup files into dir
(setq auto-save-default nil) ;; Stop creating auto #autosave# files

(global-linum-mode 1) ;; Show line-number
(setq linum-format "%d ") ;; Separating line numbers from text
;; (set-face-foreground 'linum "blue") ;; Linum foreground color

;; Disable line numbers in shell mode
(setq linum-mode-inhibit-modes-list '(shell-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)

(setq inhibit-startup-message t) ;; Disable startup messages
(tool-bar-mode -1) ;; Hide toolbar
(scroll-bar-mode -1) ;; Hide scrollbar
(global-auto-complete-mode t) ;; Enable auto-complete
;; (global-hl-line-mode +1) ;; Highlight current line
;; (set-face-background hl-line-face "magenta") ;; Background hl-line
;; (column-number-mode 1) ;; Show column number
;; (set-face-foreground 'minibuffer-prompt "red") ;; Minibuffer font color
;; (set-face-attribute 'region nil :background "#ff0000") ;; Selected region background

;; (cua-mode 1) ;; Use standard keys for undo cut copy paste

;; Resize window
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window) 
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;; Load workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)
;; (wg-load "~/.emacs.d/workgroups/group1")
;; (shell "*shell*")

;; Load theme
(require 'color-theme)
(color-theme-initialize)
;; (color-theme-lawrence)
;; (color-theme-charcoal-black)
(color-theme-renegade)

;; Parse these extensions as PHP
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . php-mode))

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
