;;;;
;;;; Additional shared FFI definitions/routines.
;;;;

(library (ffi-extensions)
  (export c-string
          create-c-string-input-port
          c-string->string
          array-of-c-strings->list
          allocate-c-string
          with-allocated-pointer)
  (import (chezscheme)
          (rvector)
          (control-flow))

  ;; Pointer to an array of char.
  (define-ftype c-string (* char))

  ;; Runs given action with allocated pointer and releases it using releaser
  ;; afterwards.
  (define (with-allocated-pointer allocator action releaser)
    (let ([pointer (allocator)])
      (unwind-protect
       (action pointer)
       (releaser pointer))))
      
  ;; Converts given c string into input port (that we can read data from).
  ;; So far it's a non-repositionable port.
  (define (create-c-string-input-port c-str)
    (let ([position 0])
      (make-custom-binary-input-port
       "native-c-string"
       (lambda (bv st n)
         (let loop ([ch (ftype-ref c-string (position) c-str)]
                    [bytes-read 0])
           (cond [(char=? ch #\nul) bytes-read]
                 [(>= bytes-read n) bytes-read]
                 [else (bytevector-u8-set! bv (+ st position) (char->integer ch))
                       (set! position (+ 1 position))
                       (loop (ftype-ref c-string (position) c-str)
                             (+ 1 bytes-read))])))
       #f
       #f
       #f)))

  ;; Converts c-string into Scheme string by byte-copying until first null.
  (define (c-string->string str)
    (call-with-port (create-c-string-input-port str)
                    (lambda (port)
                      (bytevector->string (get-bytevector-all port) (current-transcoder)))))

  ;; Converts an array represented as pointer and number of elements into a
  ;; list of strings.
  (define (array-of-c-strings->list array num)
    (do ([i 0 (+ i 1)]
         [result (list)])
        ((= i num) (reverse! result))
      (let ([c-str (ftype-&ref c-string () array i)])
        (set! result (cons (c-string->string c-str) result)))))

  ;; Allocates a fresh C-string initialized with contents of argument-string.
  (define (allocate-c-string scheme-string)
    (assert (string? scheme-string))
    (let* ([len (string-length scheme-string)]
           [c-str (make-ftype-pointer c-string
                                      (foreign-alloc (+ 1 len)))])
      (ftype-set! c-string (len) c-str #\nul)
      (format #t "~&Set null block")
      (do ([i 0 (+ i 1)]
           [ch (string-ref scheme-string 0) (string-ref scheme-string i)])
          ((>= i len) c-str)
        (ftype-set! c-string (i) c-str ch))))

  ;; Simplify setting values for foreign-type.
  (define-syntax ftype-set-values!
    (syntax-rules ()
      ([_ type-descriptor pointer (name value) ...]
       (begin (ftype-set! type-descriptor (name) pointer value)
              ...)))))
