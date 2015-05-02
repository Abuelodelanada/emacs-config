;;; drupal-spell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (drupal-spell-enable) "drupal-spell" "drupal-spell.el"
;;;;;;  (21828 51960))
;;; Generated autoloads from drupal-spell.el

(autoload 'drupal-spell-enable "drupal-spell" "\
Enable Drupal dictionary as aspell extra dictionary in current buffer.

\(fn)" t nil)

(add-hook 'drupal-mode-hook 'drupal-spell-enable)

;;;***

;;;### (autoloads nil nil ("drupal-spell-pkg.el") (21828 51960 638408))

;;;***

(provide 'drupal-spell-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; drupal-spell-autoloads.el ends here
