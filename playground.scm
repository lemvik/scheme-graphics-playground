;;;;
;;;; Interesting parts of graphics playground.
;;;;

(load-shared-object "libvulkan.so")
(load-shared-object "libglfw.so")

(top-level-program
 (import (chezscheme)
         (prefix (glfw api) glfw:)
         (prefix (vulkan api) vulkan:))

 (define vulkan-instance #f)

 (define vulkan-extensions (list "VK_EXT_debug_utils"))
 (define vulkan-layers (list "VK_LAYER_LUNARG_standard_validation"))

 (define (on-init window-ptr)
   (let ([required-extensions (append vulkan-extensions (glfw:query-enabled-extensions))])
     (let ([vulkan-result (vulkan:create-instance "Scheme-Vulkan" "No Engine" vulkan-layers required-extensions (vulkan:make-version 1 0 0))])
       (when vulkan-result
         (set! vulkan-instance vulkan-result))
       (let-values ([(vulkan-messenger callback) (vulkan:make-debug-messenger vulkan-result #x1111 #x7 (lambda (sev type mes)
                                                                                                         (format #t "~&Severity: ~a, type: ~a, message: ~a~%" sev type mes)))])
         (format #t "~&Required extensions: ~a~%Vulkan initialized: ~aMessenger initialized:~a~%" required-extensions vulkan-result vulkan-messenger)))
   (flush-output-port)))

 (define (on-destroy window-ptr)
   (vulkan:destroy-instance vulkan-instance))

 (define (frame-update window-ptr)
   #f)

 (glfw:with-window 800 600 "Scheme: graphics playground." on-init on-destroy frame-update))
