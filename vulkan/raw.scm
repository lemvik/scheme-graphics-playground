;;;;
;;;; Vulkan raw bindings for Chez Scheme.
;;;;

(library (vulkan raw)
  (export enumerate-instance-extension-properties
          create-instance

          make-version

          make-application-info
          free-application-info

          make-instance-create-info
          free-instance-create-info)
  (import (chezscheme)
          (prefix (ffi base)   ffi:)
          (prefix (ffi string) ffi:))

  ;; Returns number of available instance extensions.
  (define enumerate-instance-extension-properties
    (foreign-procedure "vkEnumerateInstanceExtensionProperties" (string u32* uptr) int))

  ;; Creates Vulkan instance and returns success code.
  (define create-instance
    (foreign-procedure "vkCreateInstance" ((* instance-create-info) uptr uptr) int))

  ;; This is definitions from VkStructureType enumeration in vulkan_core.h
  (define application-info-structure-type 0)
  (define instance-create-info-structure-type 1)

  ;; Create integer representing version according to Vulkan's rules.
  ;; Essentially a VK_MAKE_VERSION macro replica.
  (define (make-version major minor patch)
    (bitwise-ior (bitwise-arithmetic-shift-left major 22)
                 (bitwise-arithmetic-shift-left minor 12)
                 patch))

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
      (foreign-free (ftype-pointer-address instance-info-ptr)))))
