;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/parse
         axon/syntax
         racket/dict
         racket/port)

(struct http-message (headers in-port) #:transparent)

(define (http-message-printer encoders)
  (λ (msg out-port)
    (let ([headers (http-message-headers msg)])
      (dict-for-each headers
                     (λ (key val)
                       (let ([encode (or (dict-ref encoders key #f) string->bytes)])
                         (fprintf out-port "~a: ~a\r\n" key (encode val)))))
      (fprintf out-port "\r\n")
      (copy-bytes (dict-ref headers 'Content-Length #f)
                  (http-message-in-port msg) out-port))))

(define (http-message-headers-parser decoders)
  (define header-parser
    (parser [key <- (>>= (TOK #px#"^([^ :]+) *: *") bytes->symbol/titlecase)
                 !! 'HEADER-KEY]
            [val <- (TOK #px#"^([^\r]+)\r\n") !! 'HEADER-VALUE]
            [decode := (or (dict-ref decoders key #f) bytes->string)]
            (cons key (decode val))))
  (λ (in-port)
    (let loop ()
      (if (equal? #"\r\n" (peek-bytes 2 0 in-port))
          null
          (cons (header-parser in-port) (loop))))))

(define (http-message-body msg)
  (let ([len (dict-ref (http-message-headers msg) 'Content-Length #f)]
        [in-port (http-message-in-port msg)])
    (if len
        (bytes->string (read-bytes len in-port))
        (port->string in-port))))

(define http/1.1-header-value-decoders
  `([Content-Length . ,bytes->number]
    [Content-Type . ,bytes->symbol]))

(define http/1.1-header-value-encoders
  `([Content-Length . ,number->bytes]
    [Content-Type . ,symbol->bytes]))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-exn "http-message-headers-parser raises 'HEADER-KEY on bad key."
            (λ (err) (eq? err 'HEADER-KEY))
            (λ () (<<< (http-message-headers-parser null) #" K:V\r\n")))

  (test-exn "http-message-headers-parser raises 'HEADER-KEY on missing colon."
            (λ (err) (eq? err 'HEADER-KEY))
            (λ () (<<< (http-message-headers-parser null) #"K\r\n")))

  (test-exn "http-message-headers-parser raises 'HEADER-VALUE on no value."
            (λ (err) (eq? err 'HEADER-VALUE))
            (λ () (<<< (http-message-headers-parser null) #"K:\r\n")))

  (test-equal? "http-message-headers-parser reads an alist."
               (<<< (http-message-headers-parser `([X . ,bytes->symbol]
                                                   [Y . ,bytes->number]))
                    #"Y:123\r\nX:abc\r\n\r\n")
               '([Y . 123] [X . abc])))
