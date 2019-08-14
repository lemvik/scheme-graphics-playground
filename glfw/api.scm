;;;;
;;;; Scheme API wrapper around GLFW3 library.
;;;;

(library (glfw api)
  (export set-error-callback
          with-window
          query-required-extensions)

  (import (chezscheme)
          (control-flow)
          (prefix (glfw raw) glfw-raw:)
          (prefix (glfw error) glfw-error:)
          (prefix (ffi string) ffi:))

  ;; Sets error callback for the GLFW library.
  (define (set-error-callback callback)
    (let ([callable (foreign-callable callback
                                      (integer-32 string)
                                      void)])
      (lock-object callable)
      (glfw-raw:set-error-callback (foreign-callable-entry-point callable))))

  ;; Initialize GLFW library.
  (define (initialize)
    (unless (glfw-raw:initialize)
      (raide (glfw-error:make-glfw-error))))

  ;; Terminate GLFW library.
  (define (terminate)
    (glfw-raw:terminate))

  ;; Creates a glfw window and runs given functions in it.
  ;; on-init is run just before the main loop starts and is given window pointer.
  ;; frame-update is run on each frame update and is given no arguments.
  (define (with-window width height title on-init on-destroy frame-update)
    (initialize)
    (glfw-raw:window-hint glfw-raw:client-api-hint glfw-raw:no-api)
    (let ([window-ptr (glfw-raw:create-window width height title 0 0)])
      (on-init window-ptr)
      (until (glfw-raw:window-should-close window-ptr)
        (glfw-raw:poll-events)
        (frame-update window-ptr))
      (on-destroy window-ptr)
      (glfw-raw:destroy-window window-ptr))
    (terminate))

  ;; Returns a list of enabled instance extensions for Vulkan.
  (define (query-required-extensions)
    (let ([ref-container (make-bytevector 4 0)])
      (let ([extensions-strings (glfw-raw:get-required-vulkan-instance-extensions
                                 ref-container)])
        (if (ftype-pointer-null? extensions-strings)
            (raise (glfw-error:make-glfw-error))
            (let ([number-of-extensions (bytevector-u32-native-ref ref-container 0)])
              (ffi:c-strings->scheme-strings extensions-strings number-of-extensions)))))))
