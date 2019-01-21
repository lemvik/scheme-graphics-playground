;;;;
;;;; Scheme API wrapper around GLFW3 library.
;;;;


(library (glfw api)
  (export with-window
          query-enabled-extensions
          raw-enabled-extensions)

  (import (chezscheme)
          (ffi-extensions)
          (control-flow)
          (add-prefix (glfw raw) "glfw-raw:"))

  ;; Creates a glfw window and runs given functions in it.
  ;; on-init is run just before the main loop starts and is given window pointer.
  ;; frame-update is run on each frame update and is given no arguments.
  (define (with-window width height title on-init frame-update)
    (unless (glfw-raw:init)
      (error 'with-glfw-window "Failed to initialize GLFW." #f))
    (glfw-raw:window-hint glfw-raw:client-api
                          glfw-raw:no-api)
    (let ([window-ptr (glfw-raw:create-window width height title 0 0)])
      (on-init window-ptr)
      (until (glfw-raw:window-should-close window-ptr)
        (glfw-raw:poll-events))
      (glfw-raw:destroy-window window-ptr))
    (glfw-raw:terminate))

  ;; Returns a list of enabled instance extensions for Vulkan.
  (define (query-enabled-extensions)
    (let-values (([num strings] (raw-enabled-extensions)))
      (array-of-c-strings->list strings num)))

  ;; Returns values consisting of number of extensions and their name in FFI format.
  (define (raw-enabled-extensions)
    (let ([ref-container (make-bytevector 4 0)])
      (let ([extensions-strings (glfw-raw:get-required-vulkan-instance-extensions
                                 ref-container)])
        (let ([number-of-extensions (bytevector-u32-native-ref ref-container 0)])
          (values number-of-extensions
                  extensions-strings))))))