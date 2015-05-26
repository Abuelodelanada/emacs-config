(require 'generic-x)

(define-generic-mode 
  'drupal-make-syntax-mode                          ;; name of the mode
  '(";")                           ;; comments delimiter
  '("function" "var" "return")      ;; some keywords
  '(("=" . 'font-lock-operator) 
    ("+" . 'font-lock-operator)     ;; some operators
    (";" . 'font-lock-builtin))     ;; a built-in 
  '("\\.myext$")                    ;; files that trigger this mode
   nil                              ;; any other functions to call
  "My custom highlighting mode"     ;; doc string
)