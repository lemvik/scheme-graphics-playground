;;;;
;;;; Interesting parts of graphics playground.
;;;;

(top-level-program
 (import (chezscheme)
         (prefix (glfw api) glfw:))

 (define (on-init window-ptr)
   (let ([required-extensions (glfw:query-enabled-extensions)])
     (format #t "~&Required extensions: ~a~%" required-extensions))
   (flush-output-port))

 (define (frame-update window-ptr)
   #f)

 (glfw:with-window 800 600 "Scheme: graphics playground." on-init frame-update))
