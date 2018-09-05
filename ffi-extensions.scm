;;;;
;;;; Additional shared FFI definitions/routines.
;;;;

(library (ffi-extensions)
  (export c-string
          create-c-string-input-port
          c-string->string
          array-of-c-strings->list
          allocate-c-string
          allocate-c-string-array
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
  ;; Returns an address of the allocated null-terminated string.
  (define (allocate-c-string scheme-string)
    (assert (string? scheme-string))
    (let* ([len (string-length scheme-string)]
           [size (* (foreign-sizeof 'char) (+ 1 len))]
           [c-str (foreign-alloc size)])
      (foreign-set! 'char c-str len #\nul)
      (do ([i 0 (+ i 1)])
          ((>= i len) c-str)
        (let ([ch (string-ref scheme-string i)])
          (foreign-set! 'char c-str i ch)))))

  ;; Allocates an array of c-strings and returns ftype pointer to
  ;; c-string datatype.
  (define (allocate-c-string-array . strings)
    (unless (null? strings)
      (let* ([strings-count (length strings)]
             [pointer (make-ftype-pointer c-string (foreign-alloc (* (ftype-sizeof c-string)
                                                                     strings-count)))])
        (let loop ([i 0]
                   [str (allocate-c-string (car strings))]
                   [rest (cdr strings)])
          (ftype-set! c-string () pointer i (make-ftype-pointer char str))
          (if (null? rest)
              pointer
              (loop (+ i 1)
                    (allocate-c-string (car rest))
                    (cdr rest)))))))

  ;; Simplify setting values for foreign-type.
  (define-syntax ftype-set-values!
    (syntax-rules ()
      ([_ type-descriptor pointer (name value) ...]
       (begin (ftype-set! type-descriptor (name) pointer value)
              ...)))))
