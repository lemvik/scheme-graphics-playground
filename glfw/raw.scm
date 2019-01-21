;;;;
;;;; Raw bindings for GLFW.
;;;;

#!chezscheme

;;; This line is required to use glfw raw.
; (load-shared-object "libglfw.so")

(library (glfw raw)
  (export glfw-init
          glfw-terminate
          glfw-window-hint
          glfw-create-window
          glfw-destroy-window
          glfw-window-should-close
          glfw-poll-events
          glfw-get-required-instance-extensions)

  (import (chezscheme)
          (ffi-extensions))

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
                       (* c-string)))

  ;;; Hints to use.

  ;; Which client API to use.
  (define client-api-hint #x00022001)

  ;; OpenGL API
  (define opengl-api #x00030001)
  ;; OpenGL ES API
  (define opengl-es-api #x00030002)
  ;; No particular API to use (needed when Vulkan is used).
  (define no-api 0))

