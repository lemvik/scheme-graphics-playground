;;;;
;;;; Various utilities that didn't make it into R6RS or Chez Scheme.
;;;;

#!chezscheme

(library (utils)
  (export clamp)
  (import (chezscheme))

  ;; Clamps value between min-value and max-value
  (define (clamp value min-value max-value)
    (assert (<= min-value max-value))
    (min max-value
         (max min-value value))))
    
