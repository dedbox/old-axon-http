;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/http/message
         axon/parse
         racket/set)

(require [for-syntax racket/base
                     racket/syntax])

(struct http-request http-message (method target version) #:transparent)

(define (http-request-printer encoders)
  (define message-printer (http-message-printer encoders))
  (λ (msg out-port)
    (fprintf out-port "~a ~a HTTP/~a\r\n"
             (http-request-method msg)
             (http-request-target msg)
             (http-request-version msg))
    (message-printer msg out-port)))

(define http/1.1-request-printer
  (http-request-printer http/1.1-header-value-encoders))

(define http-request-methods '(GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE))

(define (http-request-parser decoders)
  (λ (in-port)
    (parse in-port
           [method <- (<* SYM SP) !! 'NO-METHOD]
           [?? (set-member? http-request-methods method) !! 'BAD-METHOD]
           [target <- (<* (!! (STOK #px#"^([^ ]+)") (λ _ 'NO-TARGET)) SP)
                   !! 'BAD-TARGET]
           [version <- (*> (TOK #"HTTP/") (<* NUM CRLF)) !! 'BAD-VERSION]
           [headers <- (<* (http-message-headers-parser decoders) CRLF)]
           (http-request headers in-port method target version))))

(define http/1.1-request-parser
  (http-request-parser http/1.1-header-value-decoders))

(define-syntax (define-http-request stx)
  (syntax-case stx ()
    [(_ method)
     (with-syntax ([http-METHOD (format-id stx "http-~a" #'method)]
                   [-http-METHOD (format-id stx "-http-~a" #'method)])
       #'(begin
           (struct http-METHOD http-request () #:transparent
                   #:name -http-METHOD #:constructor-name -http-METHOD)
           (define (http-METHOD target
                                [headers null]
                                [in-port (open-input-bytes #"")])
             (-http-METHOD headers in-port 'method target 1.1))))]))

(define-http-request GET)
(define-http-request HEAD)
(define-http-request POST)
(define-http-request PUT)
(define-http-request DELETE)
(define-http-request CONNECT)
(define-http-request OPTIONS)
(define-http-request TRACE)

;;; Unit Tests

(module+ test
  (require rackunit)

  (check-equal? (>>> http/1.1-request-printer
                     (http-request '([Content-Length . 10])
                                   (open-input-bytes #"123456789012345") 'PUT "/" 1.1))
                (bytes-append #"PUT / HTTP/1.1\r\n"
                              #"Content-Length: 10\r\n"
                              #"\r\n"
                              #"1234567890"))

  (test-case "http-request-parser reads an http-request."
             (let ([req (<<< http/1.1-request-parser
                             (bytes-append #"POST / HTTP/1.1\r\n"
                                           #"host: localhost\r\n"
                                           #"content-length: 10\r\n"
                                           #"\r\n"
                                           #"123456789012345"))])
               (check-equal? (http-message-headers req) '([Host . "localhost"]
                                                          [Content-Length . 10]))
               (check-equal? (http-message-body req) "1234567890")
               (check-eq? (http-request-method req) 'POST)
               (check-equal? (http-request-target req) "/")
               (check = (http-request-version req) 1.1)))

  (test-exn "http-request-parser raises NO-METHOD on no method."
            (λ (err) (eq? err 'NO-METHOD))
            (λ () (<<< http/1.1-request-parser #" / HTTP/1.1\r\n\r\n")))

  (test-exn "http-request-parser raises BAD-METHOD on bad method."
            (λ (err) (eq? err 'BAD-METHOD))
            (λ () (<<< http/1.1-request-parser #"FOO / HTTP/1.1\r\n\r\n")))

  (test-exn "http-request-parser raises NO-TARGET on no target."
            (λ (err) (eq? err 'NO-TARGET))
            (λ () (<<< http/1.1-request-parser #"GET  HTTP/1.1\r\n\r\n")))

  (test-exn "http-request-parser raises BAD-TARGET on bad target."
            (λ (err) (eq? err 'BAD-TARGET))
            (λ () (<<< http/1.1-request-parser #"GET /HTTP/1.1\r\n\r\n")))

  (test-exn "http-request-parser raises BAD-VERSION on bad version."
            (λ (err) (eq? err 'BAD-VERSION))
            (λ () (<<< http/1.1-request-parser #"GET / HTTP/\r\n\r\n"))))
