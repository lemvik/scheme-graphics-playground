;;;;
;;;; Vulkan Scheme-level API.
;;;;

(library (vulkan api)
  (export create-instance
          extensions-count)
  (import (chezscheme)
          (prefix (vulkan raw) vulkan-raw:))

  (define (extensions-count)
    0)

  (define (vulkan-create-instance)
    0))
