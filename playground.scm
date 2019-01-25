;;;;
;;;; Interesting parts of graphics playground.
;;;;

(top-level-program
 (import (chezscheme)
         (prefix (glfw api) glfw:)
         (prefix (vulkan api) vulkan:))

 (define (on-init window-ptr)
   (let ([required-extensions (glfw:query-enabled-extensions)])
     (let ([vulkan-result (vulkan:create-instance "Scheme-Vulkan" "No Engine" required-extensions (vulkan:make-version 1 0 0))])
       (format #t "~&Required extensions: ~a~%Vulkan initialized: ~a~%" required-extensions vulkan-result))
   (flush-output-port)))

 (define (frame-update window-ptr)
   #f)

 (glfw:with-window 800 600 "Scheme: graphics playground." on-init frame-update))
