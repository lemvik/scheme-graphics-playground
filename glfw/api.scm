;;;;
;;;; Scheme API wrapper around GLFW3 library.
;;;;

(library (glfw api)
  (export set-error-callback

          with-window
          window-width
          window-height
          window-title
          window-description

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
      (raise (glfw-error:make-glfw-error))))

  ;; Terminate GLFW library.
  (define (terminate)
    (glfw-raw:terminate))

  ;; Scheme representation of GLFW window.
  (define-record-type window
    (nongenerative)
    (fields (immutable width)
            (immutable height)
            (immutable title)
            (immutable ptr)))

  ;; Human-friendly window description.
  (define (window-description win)
    (format #f "GLFW window [title=~a][dimensions=(~ax~a)]"
            (window-title win)
            (window-width win)
            (window-height win)))

  ;; Creates a glfw window and runs given functions in it.
  ;; on-init is run just before the main loop starts and is given window pointer.
  ;; frame-update is run on each frame update and is given no arguments.
  (define (with-window width height title on-init on-destroy frame-update)
    (initialize)
    (glfw-raw:window-hint glfw-raw:client-api-hint glfw-raw:no-api)
    (let* ([win-ptr (glfw-raw:create-window width height title 0 0)]
           [win (make-window width height title win-ptr)])
      (on-init win)
      (until (glfw-raw:window-should-close win-ptr)
        (glfw-raw:poll-events)
        (frame-update win))
      (on-destroy win)
      (glfw-raw:destroy-window win-ptr))
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
