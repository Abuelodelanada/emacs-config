;;; highlight-parentheses-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-highlight-parentheses-mode highlight-parentheses-mode)
;;;;;;  "highlight-parentheses" "highlight-parentheses.el" (21843
;;;;;;  18446))
;;; Generated autoloads from highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

(defvar global-highlight-parentheses-mode nil "\
Non-nil if Global-Highlight-Parentheses mode is enabled.
See the command `global-highlight-parentheses-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-parentheses-mode'.")

(custom-autoload 'global-highlight-parentheses-mode "highlight-parentheses" nil)

(autoload 'global-highlight-parentheses-mode "highlight-parentheses" "\
Toggle Highlight-Parentheses mode in every possible buffer.
With prefix ARG, turn Global-Highlight-Parentheses mode on if and only if
ARG is positive.
Highlight-Parentheses mode is enabled in all buffers where
`(lambda nil (highlight-parentheses-mode 1))' would do it.
See `highlight-parentheses-mode' for more information on Highlight-Parentheses mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("highlight-parentheses-pkg.el") (21843
;;;;;;  18446 359172))

;;;***

(provide 'highlight-parentheses-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-parentheses-autoloads.el ends here
