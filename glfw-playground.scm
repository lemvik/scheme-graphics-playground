;;;;
;;;; Playground for debugging GLFW functions.
;;;;

(load-shared-object "/usr/local/lib/libglfw.so")

(top-level-program
 (import (chezscheme)
         (prefix (glfw api) glfw:))

 (define (on-init window)
   (format #t "~&Initializing window: ~a~%" (glfw:window-description window))
   (format #t "~&Required Vulkan extensions: ~a~%" (glfw:query-required-extensions)))

 (define (on-destroy window)
   (format #t "~&Closing window: ~a~%" (glfw:window-description window)))

 (define (on-frame window)
   #t)

 (glfw:with-window 800 600 "GLFW" on-init on-destroy on-frame))
