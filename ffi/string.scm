;;;;
;;;; FFI string utilities - mostly working with strings arrays.
;;;;

#!chezscheme

(library (ffi string)
  (export c-string

          c-string->scheme-string
          c-strings->scheme-strings

          scheme-string->c-string
          scheme-strings->c-strings

          release-c-strings)

  (import (chezscheme)
          (ffi base)
          (utils))

  ;; Pointer to an array of C characters.
  ;; Since we can't define pointer to
  (define-ftype c-string (* char))

  ;; Copies given C string into Scheme string using (current-transcoder) parameter.
  (define (c-string->scheme-string c-str)
    (call-with-port (create-c-buffer-input-port (foreign-transmute-pointer c-str unsigned-8))
                    (lambda (port)
                      (bytevector->string (get-bytevector-all port) (current-transcoder)))))

  ;; Converts an array of C strings into array of Scheme strings using c-string->scheme-string function.
  (define (c-strings->scheme-strings c-strings-array c-strings-count)
    (do ([i 0 (+ i 1)]
         [result (list)])
        ((= i c-strings-count) (reverse! result))
      (let ([c-str (ftype-&ref c-string () c-strings-array i)])
        (set! result (cons (c-string->scheme-string c-str) result)))))

  ;; Writes scheme string into given byte-buffer.
  (define (write-scheme-string-to-byte-buffer! scheme-string byte-buffer byte-buffer-size)
    (let ([bytes (string->bytevector scheme-string (current-transcoder))])
      (assert (<= (bytevector-length bytes) byte-buffer-size))
      (call-with-port (create-c-buffer-output-port byte-buffer byte-buffer-size)
                      (lambda (port)
                        (put-bytevector port bytes)))))

  ;; Allocates a C string and copies contents of the scheme string there. Returns
  ;; pointer to a null-terminated C string.
  (define (scheme-string->c-string scheme-string)
    (assert (string? scheme-string))
    (let* ([bt (string->bytevector scheme-string (current-transcoder))]
           [sl (1+ (bytevector-length bt))]
           [rp (foreign-allocate char sl)]
           [up (foreign-transmute-pointer rp unsigned-8)])
      (ftype-set! unsigned-8 () up sl 0)
      (write-scheme-string-to-byte-buffer! scheme-string up (1- sl))
      rp))

  ;; Converts a list of scheme strings into C array of pointers to char (char**) and number of those strings.
  (define (scheme-strings->c-strings scheme-strings)
    (if (null? scheme-strings)
        (values 0 0)
        (let* ([strings-number (length scheme-strings)]
               [result-pointer (foreign-allocate c-string strings-number)])
          (let loop ([i 0]
                     [str (scheme-string->c-string (car scheme-strings))]
                     [rest (cdr scheme-strings)])
            (ftype-set! c-string () result-pointer i str)
            (if (null? rest)
                (values result-pointer strings-number)
                (loop (1+ i)
                      (scheme-string->c-string (car rest))
                      (cdr rest)))))))

  ;; Releases array of C strings.
  (define (release-c-strings c-strings-pointer c-strings-count)
    (do ([i 0 (1+ i)])
        ((>= i c-strings-count) (foreign-free c-strings-pointer))
      (let ([str-pointer (ftype-ref c-string () c-strings-pointer i)])
        (foreign-free str-pointer)))))
