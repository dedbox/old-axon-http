;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/encoder
         axon/decoder
         axon/codec
         axon/syntax
         racket/port)

(require [for-syntax racket/base
                     racket/syntax])

(define-syntax (define-format stx)
  (syntax-case stx ()
    [(_ kind parser printer)
     (with-syntax ([kind-parser (format-id stx "~a-parser" #'kind)]
                   [kind-printer (format-id stx "~a-printer" #'kind)]
                   [kind-encoder (format-id stx "~a-encoder" #'kind)]
                   [kind-decoder (format-id stx "~a-decoder" #'kind)]
                   [kind-codec (format-id stx "~a-codec" #'kind)])
       #'(begin
           (define kind-parser parser)
           (define kind-printer printer)
           (define kind-encoder (encoder kind-printer))
           (define kind-decoder (decoder kind-parser))
           (define kind-codec (codec kind-parser kind-printer))))]))

(define-format string port->string (flushing display))
(define-format line read-line (flushing displayln))
(define-format sexp read (flushing writeln))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/process
           axon/stream)

  (test-true "A string-printer displays messages onto its out-port."
             (let ([out-port (open-output-string)])
               (for ([i 10]) (string-printer i out-port))
               (equal? "0123456789" (get-output-string out-port))))

  (test-true "A string-sink displays messages onto its out-port."
             (let* ([out-port (open-output-string)]
                    [f (string-encoder out-port)])
               (for ([i 10]) (f i))
               (equal? "0123456789" (get-output-string out-port))))

  (test-true "A string-stream displays messages onto its out-port."
             (let* ([out-port (open-output-string)]
                    [s (string-codec (open-input-string "") out-port)])
               (for ([i 10]) (send s i))
               (equal? "0123456789" (get-output-string out-port))))

  (test-true "A string-parser returns the contents of its in-port as a string."
             (equal? "a\nb\nc" (string-parser (open-input-string "a\nb\nc"))))

  (test-true "A string-source returns the contents of its in-port as a string."
             (let* ([in-port (open-input-string "a\nb\nc")]
                    [f (string-decoder in-port)])
               (equal? "a\nb\nc" (f))))

  (test-true "A string-sream returns the contents of its in-port as a string."
             (let* ([in-port (open-input-string "a\nb\nc")]
                    [s (string-codec in-port (open-output-string))])
               (let ([msg (recv s)])
                 (equal? "a\nb\nc" msg)))))
