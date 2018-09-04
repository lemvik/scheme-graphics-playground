;;;;
;;;; Control-flow utilities.
;;;;

(library (control-flow)
  (export until
          unwind-protect)
  (import (chezscheme))

  ;; Common Lisp utility (try-finally)
  (define-syntax unwind-protect
    (syntax-rules ()
      [(_ body cleanup ...)
       (dynamic-wind
         (lambda () #f)
         (lambda () body)
         (lambda () cleanup ...))]))

  ;; Run body until condition fails.
  (define-syntax until
    (syntax-rules ()
      [(_ cond-check body ...)
       (do ()
           (cond-check #t)
         body ...)])))

