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
(menu-bar-mode -1) ;; Hide menubar
(column-number-mode 1) ;; Show column number
(global-auto-complete-mode t) ;; Enable auto-complete
(global-highlight-parentheses-mode t)
(fullscreen-mode 1)

;; (load-file "~/.emacs.d/emacs-for-python/epy-init.el") ;; Python Emacs collection

(require 'generic-x)
(define-generic-mode 'drupal-make-syntax-mode
  '(";")
  '("projects" "libraries" "dependencies" "api" "core" "name" "description")
  '(("=" . 'font-lock-operator)
    ("\\[\\(.*\\)\\]" . font-lock-type-face))
  '("\\.make$" "\\.info$")
   nil
  "Drupal make syntax mode")

(define-generic-mode 'htaccess-mode
      '(?#)
      '(;; core
        "AcceptPathInfo" "AccessFileName" "AddDefaultCharset" "AddOutputFilterByType"
        "AllowEncodedSlashes" "AllowOverride" "AuthName" "AuthType"
        "CGIMapExtension" "ContentDigest" "DefaultType" "DocumentRoot"
        "EnableMMAP" "EnableSendfile" "ErrorDocument" "ErrorLog"
        "FileETag" "ForceType" "HostnameLookups" "IdentityCheck"
        "Include" "KeepAlive" "KeepAliveTimeout" "LimitInternalRecursion"
        "LimitRequestBody" "LimitRequestFields" "LimitRequestFieldSize" "LimitRequestLine"
        "LimitXMLRequestBody" "LogLevel" "MaxKeepAliveRequests" "NameVirtualHost"
        "Options" "Require" "RLimitCPU" "RLimitMEM"
        "RLimitNPROC" "Satisfy" "ScriptInterpreterSource" "ServerAdmin"
        "ServerAlias" "ServerName" "ServerPath" "ServerRoot"
        "ServerSignature" "ServerTokens" "SetHandler" "SetInputFilter"
        "SetOutputFilter" "TimeOut" "UseCanonicalName"
        ;; .htaccess tutorial
        "AddHandler" "AuthUserFile" "AuthGroupFile"
        ;; mod_rewrite
        "RewriteBase" "RewriteCond" "RewriteEngine" "RewriteLock" "RewriteLog"
        "RewriteLogLevel" "RewriteMap" "RewriteOptions" "RewriteRule"
        ;; mod_alias
        "Alias" "AliasMatch" "Redirect" "RedirectMatch" "RedirectPermanent"
        "RedirectTemp" "ScriptAlias" "ScriptAliasMatch")
      '(("%{\\([A-Z_]+\\)}" 1 font-lock-variable-name-face)
        ("\\b[0-9][0-9][0-9]\\b" . font-lock-constant-face)
        ("\\[.*\\]" . font-lock-type-face))
      '(".htaccess\\'")
      nil
      "Generic mode for Apache .htaccess files.")

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

;; Dired

(require 'dired-sort-map)
(setq dired-listing-switches "--group-directories-first -alh")

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(put 'dired-find-alternate-file 'disabled nil) ;; Reuse directory buffer

;; Hooks

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;(add-hook 'emacs-startup-hook (lambda () (if window-system (multi-term))))

;; Themes

;(if window-system
;    (load-file "~/.emacs.d/themes/atom-dark-theme.el"))

;; Shortcuts

(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c d") 'duplicate-current-line)
(global-set-key (kbd "C-c C-z") 'term-stop-subjob)
(global-set-key (kbd "C-c C-w") 'copy-word)
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Alias

(defalias 'bkr 'browse-kill-ring)
(defalias 'lb 'list-buffers)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'tt 'tramp-term)

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

(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (let ((filep (buffer-file-name)))
    (kill-buffer (current-buffer))
    (if filep (find-file (concat "/sudo::" filep))
      (message "Current buffer does not have an associated file."))))


;;;;;;;;;;;;;;;;; Customizaciones José  ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(set-default-font "Ubuntu Mono 11")

;; Acentos
(require 'iso-transl)

; Goto-line short-cut key
(global-set-key "\C-l" 'goto-line)

; ctags
(setq path-to-ctags "/usr/bin/ctags") ;; <- your ctags path here
(defun crear-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R --languages='php' --exclude='cache' %s" path-to-ctags dir-name (directory-file-name dir-name)))
)

; Case sensitive TAGS search
(set-default 'case-fold-search nil)

;; Create tags shortcut
(global-set-key "\C-t" 'crear-tags)

;; Ir al cierre-apertura de ([{
(global-set-key "%" 'match-paren)

;;; Comentar regiones marcadas (C-c c):
(global-set-key (kbd "C-c c") 'comment-region)
;;; Descomentar regiones marcadas (C-c u):
(global-set-key (kbd "C-c u") 'uncomment-region)

; Theme
(load-theme 'monokai t)
;(add-to-list 'default-frame-alist '(background-color . "#000000"))

;; keyboard scroll one line at a time
(setq scroll-step 1)

;; Ir al cierre-apertura de ([{
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Copiar línea
(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables.  This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (toggle-read-only 1)
  (kill-line arg)
  (toggle-read-only 0))

(setq-default kill-read-only-ok t)
(global-set-key "\C-c\C-k" 'copy-line)


;; Yasnippet
(add-to-list 'load-path
             "~/.emacs.d/elpa/yasnippet-20150415.244/")
(require 'yasnippet)
(yas-global-mode 1)


(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

;;; Eliminar espacios en blanco al final de la linea automáticamente al guardar el archivo:
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)

;; ido mode
(ido-mode 1)
(setq ido-enable-flex-matching t)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Atajos para moverme entre ventanas con SHIFT
(windmove-default-keybindings)

;; Emacs code browser
(require 'ecb)
(require 'ecb-autoloads)

(global-set-key (kbd "C-c a") 'ecb-activate)
(global-set-key (kbd "C-c d") 'ecb-deactivate)
(setq ecb-layout-name "left6")
(setq ecb-tip-of-the-day nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes
   (quote
    (("left6"
      (ecb-sources-buffer-name 0.22784810126582278 . 0.1875)
      (ecb-methods-buffer-name 0.22784810126582278 . 0.5833333333333334)
      (ecb-history-buffer-name 0.22784810126582278 . 0.20833333333333334)))))
 '(ecb-options-version "2.40")
 '(php-mode-psr2-hook (quote (ignore))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




;; Markdown Modes
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(put 'upcase-region 'disabled nil)


;; YAML-MODe
(require 'yaml-mode)

;; iedit
(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(global-set-key (kbd "C-;") 'iedit-dwim)
(put 'downcase-region 'disabled nil)

;(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;; Smart Scan lets you jump between symbols in your buffer
(global-smartscan-mode 1)
