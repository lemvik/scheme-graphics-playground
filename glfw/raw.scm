;;;;
;;;; Raw bindings for GLFW.
;;;;

#!chezscheme

;;; This line is required to use glfw raw.
                                        ; (load-shared-object "libglfw.so")

(library (glfw raw)
  (export initialize
          terminate
          window-hint
          create-window
          destroy-window
          window-should-close
          poll-events
          get-required-vulkan-instance-extensions

          get-error
          set-error-callback

          client-api-hint
          opengl-api
          opengl-es-api
          no-api)

  (import (chezscheme)
          (prefix (ffi base) ffi:)
          (prefix (ffi string) ffi:))

  ;; Sets an error callback that will be invoked with error code and description each time there is an error.
  (define set-error-callback (foreign-procedure "glfwSetErrorCallback" (uptr) void))

  ;; Returns most recent error code and description fo an error.
  (define get-error-native (foreign-procedure "glfwGetError" ((* ffi:c-string)) int))


  ;; Initialize GLFW, needs to be called before any other function.
  (define initialize (foreign-procedure "glfwInit" () boolean))

  ;; Terminate GLFW, needs to be called to release any used resources.
  (define terminate (foreign-procedure "glfwTerminate" () void))

  ;; Set a window hint for window handler. First argument is hint type, second is
  ;; hint value.
  (define window-hint (foreign-procedure "glfwWindowHint" (int int) void))

  ;; Creates a GLFW window.
  ;; Arguments are: width, height, title, monitor handle, shared resources handle.
  (define create-window (foreign-procedure "glfwCreateWindow"
                                           (int int string uptr uptr)
                                           uptr))

  ;; Destroys given GLFW window.
  (define destroy-window (foreign-procedure "glfwDestroyWindow" (uptr) void))

  ;; Returns true if given GLFW window should be closed.
  (define window-should-close (foreign-procedure "glfwWindowShouldClose"
                                                 (uptr)
                                                 boolean))

  ;; Polls window events.
  (define poll-events (foreign-procedure "glfwPollEvents" () void))

  ;; Returns number and names of required Vulkan extensions. Those should be passed to
  ;; Vulkan initialization functions to set up rendering window.
  (define get-required-vulkan-instance-extensions
    (foreign-procedure "glfwGetRequiredInstanceExtensions"
                       (u32*)
                       (* ffi:c-string)))

  ;; Returns latest glfw error.
  (define (get-error)
    (ffi:with-pointer-to-c-string (lambda (description-ptr)
                                    (let ([error-code (get-error-native description-ptr)])
                                      (let ([strings (ffi:c-strings->scheme-strings description-ptr 1)])
                                        (values error-code (car strings)))))))

  ;;; Hints to use.

  ;; Which client API to use.
  (define client-api-hint #x00022001)

  ;; OpenGL API
  (define opengl-api #x00030001)
  ;; OpenGL ES API
  (define opengl-es-api #x00030002)
  ;; No particular API to use (needed when Vulkan is used).
  (define no-api 0))
