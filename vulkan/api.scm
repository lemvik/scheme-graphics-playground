;;;;
;;;; Vulkan Scheme-level API.
;;;;

(library (vulkan api)
  (export create-instance
          destroy-instance
          make-debug-messenger
          make-version)
  (import (chezscheme)
          (prefix (vulkan raw) vulkan-raw:))

  ;; Re-export version creation routine.
  (define make-version vulkan-raw:make-version)

  ;; Destroys Vulkan instance.
  (define (destroy-instance instance-ptr)
    (vulkan-raw:destroy-instance instance-ptr 0))

  ;; Creates vulkan instance and returns numeric value of the pointer to the instance.
  (define (create-instance app-name engine-name layers extensions version)
    (let* ([app-info (vulkan-raw:make-application-info app-name version engine-name version vulkan-raw:api-version)]
           [create-inst (vulkan-raw:make-instance-create-info app-info layers extensions)]
           [instance-ptr (make-ftype-pointer vulkan-raw:instance (foreign-alloc (foreign-sizeof 'uptr)))])
      (let ([result (vulkan-raw:create-instance create-inst 0 instance-ptr)])
        (vulkan-raw:free-instance-create-info create-inst)
        (vulkan-raw:free-application-info app-info)
        instance-ptr)))

  ;; Re-export raw function for the time being.
  (define make-debug-messenger vulkan-raw:make-debug-messenger))
