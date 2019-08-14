;;;;
;;;; Errors that glfw bindings might produce.
;;;;

(library (glfw error)
  (export make-glfw-error
          glfw-error?)
  (import (chezscheme)
          (prefix (glfw raw) raw:))

  ;; Condition describing error in GLFW library.
  (define-condition-type &glfw-error &violation make-glfw-error-internal glfw-error?
    (internal-code glfw-error-code))

  ;; Creates glfw error condition from recent GLFW library error.
  (define (make-glfw-error)
    (let-values ([(code description) (raw:get-error)])
      (condition (make-glfw-error-internal code)
                 (make-message-condition description)))))
