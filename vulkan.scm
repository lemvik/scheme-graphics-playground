;;;;
;;;; Vulkan API bindings for Scheme.
;;;;

#!chezscheme

(load-shared-object "libvulkan.so")

(library (vulkan)
  (export extensions-count
          create-instance

          make-version
          make-application-info
          make-create-instance-info)
  (import (chezscheme)
          (ffi-extensions)
          (control-flow))

  ;; Defines version of vulkan API to use (per docs)
  (define (make-version major minor patch)
    (bitwise-ior (bitwise-arithmetic-shift-left major 22)
                 (bitwise-arithmetic-shift-left minor 12)
                 patch))

  ;; Type of application info from Vulkan API.
  (define +vulkan-structure-type-application-info+ 0)

  ;; Scheme-side application info.
  (define-record-type application-info
    (fields (immutable app-name)
            (immutable app-version)
            (immutable engine-name)
            (immutable engine-version)
            (immutable api-version)))

  ;; Scheme-side instance creation info.
  (define-record-type create-instance-info
    (fields (immutable flags)
            (immutable app-info)
            (immutable layers)
            (immutable extensions)))

  ;; Structure of application info.
  (define-ftype vulkan-application-info
    (struct
      [structure-type int]
      [next-structure uptr]
      [application-name c-string]
      [application-version unsigned-32]
      [engine-name c-string]
      [engine-version unsigned-32]
      [api-version unsigned-32]))

  ;; Structure of instance create info.
  (define-ftype vulkan-instance-create-info
    (struct
      [structure-type int]
      [next-structure uptr]
      [flags int]
      [application-info (* vulkan-application-info)]
      [enabled-layers-count int]
      [enabled-layers-names (* c-string)]
      [enabled-extension-count int]
      [enabled-extension-names (* c-string)]))

  ;; Runs a chunk of code with memory allocated for external consumption
  ;; that holds the create instance info.
  (define (with-allocated-create-instance-info create-info action)
    (let ([app-info (create-instance-info-app-info create-info)])
      (with-allocated-app-info app-info
                               (lambda (app-info-pointer)
                                 (let ([pointer (make-ftype-pointer vulkan-instance-create-info
                                                                    (foreign-alloc (ftype-sizeof vulkan-instance-create-info)))])
                                   (doc

  
  ;; Allocates application info for
  (define (create-application-info app-name app-version engine-name engine-version)
    (let ([app-info-ptr (make-ftype-pointer vulkan-application-info
                                            (foreign-alloc (ftype-sizeof vulkan-application-info)))])
      (ftype-set-values! vulkan-application-info app-info-ptr
                         (structure-type +vulkan-structure-type-application-info+)
                         (next-structure 0)
                         (application-name app-name)
                         (application-version app-version)
                         (engine-name engine-name)
                         (engine-version engine-version)
                         (api-version (make-version 1 1 0)))
      app-info-ptr))

  ;; Runs action with given application info.
  (define-syntax with-application-info
    (syntax-rules ()
      ([_ app-name app-version engine-name engine-version action]
       (with-allocated-pointer
        (lambda ()
          (let ([name-c-string (make-ftype-pointer char (allocate-c-string app-name))]
                [engine-c-string (make-ftype-pointer char (allocate-c-string engine-name))])
            (create-application-info name-c-string app-version engine-c-string engine-version)))
        action
        (lambda (ptr)
          (let ([name-c-string (ftype-&ref vulkan-application-info (application-name) ptr)]
                [engine-c-string (ftype-&ref vulkan-application-info (engine-name) ptr)])
            ;(foreign-free (ftype-pointer-address name-c-string))
            ;(foreign-free (ftype-pointer-address engine-c-string))
            (foreign-free (ftype-pointer-address ptr))))))))

  ;; Type of instance create info structure.
  (define +vulkan-structure-type-instance-create-info+ 1)


  ;; Create instance info.
  (define (create-instance-create-info app-info-ptr extensions-count extensions-names)
    (let ([create-info-ptr (make-ftype-pointer vulkan-instance-create-info
                                               (foreign-alloc (ftype-sizeof vulkan-instance-create-info)))])
      (ftype-set-values! vulkan-instance-create-info create-info-ptr
                         (structure-type +vulkan-structure-type-instance-create-info+)
                         (next-structure 0)
                         (flags 0)
                         (application-info app-info-ptr)
                         (enabled-layers-count 0)
                         (enabled-layers-names (make-ftype-pointer c-string 0))
                         (enabled-extension-count extensions-count)
                         (enabled-extension-names extensions-names))
      create-info-ptr))

  ;; Pointer to an array of char.
  ;; Returns number of available instance extensions.
  (define vulkan-enumerate-instance-extension-properties
    (foreign-procedure "vkEnumerateInstanceExtensionProperties" (string u32* uptr) int))

  ;; Creates Vulkan instance and returns success code.
  (define vulkan-create-instance
    (foreign-procedure "vkCreateInstance" ((* vulkan-instance-create-info) uptr uptr) int))

  ;; Returns number of instance extensions available.
  (define (extensions-count)
    (let ([ref-container (make-bytevector 4 0)])
      (vulkan-enumerate-instance-extension-properties #f ref-container 0)
      (bytevector-u32-native-ref ref-container 0)))

  ;; Creates a Vulkan instance with given parameters.
  (define (create-instance app-name app-version engine-name engine-version extensions-count extensions-names-ptr)
    (with-application-info app-name app-version engine-name engine-version
      (lambda (app-info-ptr)
        (with-allocated-pointer
         (lambda ()
           (create-instance-create-info app-info-ptr extensions-count extensions-names-ptr))
         (lambda (create-info-ptr)
           (let ([instance-ptr 0])
             (vulkan-create-instance create-info-ptr 0 instance-ptr)
             instance-ptr))
         (lambda (ptr)
           (foreign-free (ftype-pointer-address ptr))))))))
