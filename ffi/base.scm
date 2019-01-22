;;;;
;;;; Basic FFI utilities for ChezScheme.
;;;;

#!chezscheme

(library (ffi base)
  (export foreign-allocate
          foreign-transmute-pointer

          ftype-set-values!

          create-c-buffer-input-port
          create-c-buffer-output-port)
  (import (chezscheme)
          (utils))

  ;; Auxiliary syntax to simplify array allocation.
  ;; Returns foreign pointer of specified type pointing to space sufficient to
  ;; hold count elements of type.
  (define-syntax foreign-allocate
    (syntax-rules ()
      ([_ type]
       (foreign-allocate type 1))
      ([_ type count]
       (make-ftype-pointer type
                           (foreign-alloc (* (ftype-sizeof type)
                                             count))))))

  ;; Returns new ftype pointer pointing to the same address but having different type.
  ;; Equivalent of a cast in C.
  (define-syntax foreign-transmute-pointer
    (syntax-rules ()
      ([_ ptr target-type]
       (make-ftype-pointer target-type (ftype-pointer-address ptr)))))
    
  ;; Converts given C string into input port (that we can read data from).
  ;; So far it's a non-repositionable port.
  (define (create-c-buffer-input-port c-str)
    (let ([position 0]
          [read-end #f])
      (make-custom-binary-input-port
       "native-c-buffer"
       (lambda (bv st n)
         (if read-end
             0
             (let loop ([ch (ftype-ref unsigned-8 () c-str position)]
                        [bytes-read 0])
               (cond ([= ch 0]
                      (set! read-end #t)
                      bytes-read)
                     ([>= bytes-read n]
                      (set! position (+ position bytes-read))
                      bytes-read)
                     (else (bytevector-u8-set! bv (+ st bytes-read) ch)
                           (loop (ftype-ref unsigned-8 () c-str (+ position bytes-read 1))
                                 (1+ bytes-read)))))))
       #f
       #f
       #f)))

  ;; Creates an output port from pre-allocated C buffer.
  (define (create-c-buffer-output-port c-buffer c-buffer-size)
    (let ([position 0])
      (make-custom-binary-output-port
       "native-c-buffer"
       (lambda (bv st n)
         (let ([bytes-to-write (clamp 0 n (- c-buffer-size position))])
           (do ([i 0 (1+ i)])
               ([>= i bytes-to-write] (begin
                                        (set! position (+ position bytes-to-write))
                                        bytes-to-write))
             (ftype-set! unsigned-8 
                         ()
                         c-buffer
                         (+ i position) 
                         (bytevector-u8-ref bv (+ st i))))))
       #f
       #f
       #f)))

  ;; Simplify setting values for foreign-type.
  (define-syntax ftype-set-values!
    (syntax-rules ()
      ([_ type-descriptor pointer (name value) ...]
       (begin (ftype-set! type-descriptor (name) pointer value)
              ...)))))
