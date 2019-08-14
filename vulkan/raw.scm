;;;;
;;;; Vulkan raw bindings for Chez Scheme.
;;;;

(library (vulkan raw)
  (export create-instance
          destroy-instance
          instance

          make-version

          message-severity-verbose
          message-severity-info
          message-severity-warning
          message-severity-error
          message-type-general
          message-type-validation
          message-type-performance

          debug-messenger-create-info
          debug-messenger-callback-data
          debug-messenger
          make-debug-messenger

          make-application-info
          free-application-info

          make-instance-create-info
          free-instance-create-info

          success-code

          api-version)
  (import (chezscheme)
          (prefix (ffi base)   ffi:)
          (prefix (ffi string) ffi:))

  ;; Instance is a opaque pointer.
  (define-ftype instance uptr)

  ;; Instance procedures lookup procedure.
  (define get-instance-proc-address
    (foreign-procedure "vkGetInstanceProcAddr" ((& instance) string) uptr))

  ;; Creates Vulkan instance and returns success code.
  (define create-instance
    (foreign-procedure "vkCreateInstance" ((* instance-create-info) uptr (* instance)) int))

  ;; Destroys Vulkan instance and returns success code.
  (define destroy-instance
    (foreign-procedure "vkDestroyInstance" ((& instance) uptr) int))

  ;; Looks up address of vkCreateDebugUtilsMessengerEXT procedure in given instance.
  (define (lookup-create-debug-messenger inst)
    (let ([proc-address (get-instance-proc-address inst "vkCreateDebugUtilsMessengerEXT")])
      (foreign-procedure proc-address ((& instance) (* debug-messenger-create-info) uptr (* debug-messenger)) int)))

  ;; Looks up address of vkDestroyDebugUtilsMessengerEXT procedure in given instance.
  (define (lookup-destroy-debug-messenger inst)
    (let ([proc-address (get-instance-proc-address inst "vkDestroyDebugUtilsMessengerEXT")])
      (foreign-procedure proc-address ((& instance) (& debug-messenger) uptr) int)))

  ;; Create integer representing version according to Vulkan's rules.
  ;; Essentially a VK_MAKE_VERSION macro replica.
  (define (make-version major minor patch)
    (bitwise-ior (bitwise-arithmetic-shift-left major 22)
                 (bitwise-arithmetic-shift-left minor 12)
                 patch))

  ;; Returns codes from Vulkan.
  (define success-code 0)

  ;; API version (stolen from vulkan_core.h on my machine).
  (define api-version (make-version 1 0 0))

  ;; This is definitions from VkStructureType enumeration in vulkan_core.h
  (define application-info-structure-type 0)
  (define instance-create-info-structure-type 1)
  (define create-debug-messenger-structure-type 1000128004)

  ;; Structure VkApplicationInfo from vulkan headers.
  (define-ftype application-info
    (struct
      [structure-type int]
      [next-structure uptr]
      [application-name ffi:c-string]
      [application-version unsigned-32]
      [engine-name ffi:c-string]
      [engine-version unsigned-32]
      [api-version unsigned-32]))

  ;; Creates application-info structure.
  (define (make-application-info app-name app-version engine-name engine-version api-version)
    (let ([app-c-string (ffi:scheme-string->c-string app-name)]
          [engine-c-string (ffi:scheme-string->c-string engine-name)]
          [app-info-ptr (ffi:foreign-allocate application-info)])
      (ffi:ftype-set-values! application-info app-info-ptr
                             [structure-type application-info-structure-type]
                             [next-structure 0]
                             [application-name app-c-string]
                             [application-version app-version]
                             [engine-name engine-c-string]
                             [engine-version engine-version]
                             [api-version api-version])
      app-info-ptr))

  ;; Releases application info structure.
  (define (free-application-info app-info-ptr)
    (let ([app-c-string (ftype-ref application-info (application-name) app-info-ptr)]
          [engine-c-string (ftype-ref application-info (engine-name) app-info-ptr)])
      (foreign-free (ftype-pointer-address app-c-string))
      (foreign-free (ftype-pointer-address engine-c-string))
      (foreign-free (ftype-pointer-address app-info-ptr))))

  ;; Structure VkInstanceCreateInfo from vulkan headers.
  (define-ftype instance-create-info
    (struct
      [structure-type int]
      [next-structure uptr]
      [instance-create-flags unsigned-32]
      [application-info (* application-info)]
      [layers-count unsigned-32]
      [layers-names (* ffi:c-string)]
      [extensions-count unsigned-32]
      [extensions-names (* ffi:c-string)]))

  ;; Creates instance-create-info structure.
  (define (make-instance-create-info app-info-ptr layers extensions)
    (let-values (([c-layers layers-count] (ffi:scheme-strings->c-strings layers))
                 ([exts exts-count] (ffi:scheme-strings->c-strings extensions)))
      (let ([instance-info-ptr (ffi:foreign-allocate instance-create-info)])
        (ffi:ftype-set-values! instance-create-info instance-info-ptr
                               [structure-type instance-create-info-structure-type]
                               [next-structure 0]
                               [instance-create-flags 0]
                               [application-info app-info-ptr]
                               [layers-count layers-count]
                               [layers-names c-layers]
                               [extensions-count exts-count]
                               [extensions-names exts])
        instance-info-ptr)))

  ;; Releases instance-create-info structure.
  (define (free-instance-create-info instance-info-ptr)
    (let ([layers-count (ftype-ref instance-create-info (layers-count) instance-info-ptr)]
          [layers-c-strings (ftype-ref instance-create-info (layers-names) instance-info-ptr)]
          [exts-count (ftype-ref instance-create-info (extensions-count) instance-info-ptr)]
          [exts-c-strings (ftype-ref instance-create-info (extensions-names) instance-info-ptr)])
      (ffi:release-c-strings layers-c-strings layers-count)
      (ffi:release-c-strings exts-c-strings exts-count)
      (foreign-free (ftype-pointer-address instance-info-ptr))))

  ;; Messages severity.
  (define message-severity-verbose #x1)
  (define message-severity-info    #x10)
  (define message-severity-warning #x100)
  (define message-severity-error   #x1000)

  ;; Message types.
  (define message-type-general     #x1)
  (define message-type-validation  #x2)
  (define message-type-performance #x4)

  ;; Callback data for debug messenger.
  (define-ftype debug-messenger-callback-data
    (struct
      [structure-type int]
      [next-structure int]
      [flats int]
      [message-id-name (* char)]
      [message-id-number integer-32]
      [message (* char)]
      [queue-labels-count unsigned-32]
      [queue-labels uptr]
      [buf-labels-count unsigned-32]
      [buf-labels uptr]
      [objects-count unsigned-32]
      [objects uptr]))

  ;; Type of debugging callback for Vulkan.
  (define-ftype debug-callback-type
    (function (int int (* debug-messenger-callback-data) uptr) boolean))

  ;; Structure describing desired debug messenger structure.
  (define-ftype debug-messenger-create-info
    (struct
      [structure-type int]
      [next-structure uptr]
      [flags int]
      [message-severity int]
      [message-type int]
      [callback (* debug-callback-type)]
      [user-data uptr]))

  ;; Opaque pointer for debug messenger.
  (define-ftype debug-messenger uptr)

  ;; Creates debug messenger for Vulkan.
  (define (make-debug-messenger inst severity-mask type-mask logger)
    (define (callback severity type callback-data ignored)
      (let ([message (ffi:c-string->scheme-string (ftype-ref debug-messenger-callback-data (message) callback-data))])
        (logger severity type message)))

    (let ([create-debug-messenger (lookup-create-debug-messenger inst)]
          [callable (foreign-callable callback
                                      (integer-32 integer-32 (* debug-messenger-callback-data) uptr)
                                      boolean)])
      (lock-object callable)
      (let ([create-structure (ffi:foreign-allocate debug-messenger-create-info)]
            [messenger (ffi:foreign-allocate debug-messenger)]
            [callable-pointer (make-ftype-pointer debug-callback-type (foreign-callable-entry-point callable))])

        (ffi:ftype-set-values! debug-messenger-create-info create-structure
                               [structure-type create-debug-messenger-structure-type]
                               [next-structure 0]
                               [flags 0]
                               [message-severity severity-mask]
                               [message-type type-mask]
                               [callback callable-pointer]
                               [user-data 0])
        (let ([result (create-debug-messenger inst create-structure 0 messenger)])
          (values messenger callback result))))))
