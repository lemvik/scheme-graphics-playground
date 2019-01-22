;;;;
;;;; Interesting pargs of graphics playground.
;;;;

(top-level-program
 (import (chezscheme)
         (prefix (glfw api) glfw:))

 (load-shared-object "libglfw.so")

 (define (on-init window-ptr)
   (display "Initialized GLFW")
   (flush-output-port))

 (define (frame-update window-ptr)
   #f)

 (glfw:with-window 800 600 "Scheme: graphics playground." on-init frame-update))
