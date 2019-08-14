;;;;
;;;; Playground for debugging GLFW functions.
;;;;

(load-shared-object "/usr/local/lib/libglfw.so")

(top-level-program
 (import (chezscheme)
         (prefix (glfw api) glfw:))

 (define (on-init window-ptr)
   (format #t "~&Initializing window: ~a~%" window-ptr)
   (format #t "~&Required Vulkan extensions: ~a~%" (glfw:query-required-extensions)))

 (define (on-destroy window-ptr)
   (format #t "~&Closing window: ~a~%" window-ptr))

 (define (on-frame window-ptr)
   #t)

 (glfw:with-window 800 600 "GLFW" on-init on-destroy on-frame))
