;;;;
;;;; GLFW wrapper for Chez scheme.
;;;;

(load-shared-object "libglfw.so")

(library (glfw)
  (export with-glfw-window
          query-enabled-extensions
          raw-enabled-extensions)
  (import (chezscheme)
          (ffi-extensions)
          (control-flow))

  (define glfw-init (foreign-procedure "glfwInit" () boolean))
  (define glfw-terminate (foreign-procedure "glfwTerminate" () void))
  (define glfw-window-hint (foreign-procedure "glfwWindowHint" (int int) void))
  (define glfw-create-window (foreign-procedure "glfwCreateWindow" (int int string uptr uptr) uptr))
  (define glfw-destroy-window (foreign-procedure "glfwDestroyWindow" (uptr) void))
  (define glfw-window-should-close (foreign-procedure "glfwWindowShouldClose" (uptr) boolean))
  (define glfw-poll-events (foreign-procedure "glfwPollEvents" () void))
  (define glfw-get-required-instance-extensions (foreign-procedure "glfwGetRequiredInstanceExtensions" (u32*) (* c-string)))

  #!chezscheme
  (define +glfw-client-api+ #x00022001)
  (define +glfw-no-api+ 0)

  ;; Creates a glfw window and runs given functions in it.
  ;; on-init is run just before the main loop starts and is given window pointer.
  ;; frame-update is run on each frame update and is given no arguments.
  (define (with-glfw-window width height title on-init frame-update)
    (format #t "~&Creating GLFW window (~a, ~a) with title ~a~%" width height title)
    (unwind-protect
     (begin (glfw-init)
            (glfw-window-hint +glfw-client-api+ +glfw-no-api+)
            (let ([window-pointer (glfw-create-window width height title 0 0)])
              (on-init window-pointer)
              (until (glfw-window-should-close window-pointer)
                (glfw-poll-events))
              (glfw-destroy-window window-pointer)))
     (glfw-terminate)))

  ;; Returns a list of enabled instance extensions for Vulkan.
  (define (query-enabled-extensions)
    (let-values (([num strings] (raw-enabled-extensions)))
      (array-of-c-strings->list strings num)))

  ;; Returns values consisting of number of extensions and their name in FFI format.
  (define (raw-enabled-extensions)
    (let ([ref-container (make-bytevector 4 0)])
      (let ([extensions-strings (glfw-get-required-instance-extensions ref-container)])
        (let ([number-of-extensions (bytevector-u32-native-ref ref-container 0)])
          (values number-of-extensions
                  extensions-strings))))))
