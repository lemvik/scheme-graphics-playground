;;;;
;;;; Scheme API wrapper around GLFW3 library.
;;;;

(library (glfw api)
  (export with-window
          query-enabled-extensions)

  (import (chezscheme)
          (control-flow)
          (prefix (glfw raw) glfw-raw:)
          (prefix (ffi string) ffi:))

  ;; Creates a glfw window and runs given functions in it.
  ;; on-init is run just before the main loop starts and is given window pointer.
  ;; frame-update is run on each frame update and is given no arguments.
  (define (with-window width height title on-init on-destroy frame-update)
    (unless (glfw-raw:initialize)
      (error 'with-glfw-window "Failed to initialize GLFW." #f))
    (glfw-raw:window-hint glfw-raw:client-api-hint
                          glfw-raw:no-api)
    (let ([window-ptr (glfw-raw:create-window width height title 0 0)])
      (on-init window-ptr)
      (until (glfw-raw:window-should-close window-ptr)
        (glfw-raw:poll-events)
        (frame-update window-ptr))
      (on-destroy window-ptr)
      (glfw-raw:destroy-window window-ptr))
    (glfw-raw:terminate))

  ;; Returns a list of enabled instance extensions for Vulkan.
  (define (query-enabled-extensions)
    (let ([ref-container (make-bytevector 4 0)])
      (let ([extensions-strings (glfw-raw:get-required-vulkan-instance-extensions
                                 ref-container)])
        (if (ftype-pointer-null? extensions-strings)
            (list)
            (let ([number-of-extensions (bytevector-u32-native-ref ref-container 0)])
              (ffi:c-strings->scheme-strings extensions-strings number-of-extensions)))))))
