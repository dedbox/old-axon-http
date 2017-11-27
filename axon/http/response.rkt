;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/http/message
         axon/parse)

(require [for-syntax racket/base
                     racket/syntax])

(struct http-response http-message (status reason version) #:transparent)

(define (http-response-printer encoders)
  (define message-printer (http-message-printer encoders))
  (λ (msg out-port)
    (fprintf out-port "HTTP/~a ~a ~a\r\n"
             (http-response-version msg)
             (http-response-status msg)
             (http-response-reason msg))
    (message-printer msg out-port)))

(define http/1.1-response-printer
  (http-response-printer http/1.1-header-value-encoders))

(define (http-response-parser decoders)
  (λ (in-port)
    (parse in-port
           [version <- (*> (TOK #"HTTP/") NUM) !! 'BAD-VERSION]
           [status <- (*> SP (<* NUM SP)) !! 'BAD-STATUS]
           [reason <- (STOK #px#"^([^\r]+)\r\n") !! 'BAD-REASON]
           [headers <- (<* (http-message-headers-parser decoders) CRLF)]
           (http-response headers in-port status reason version))))

(define-syntax (define-http-response stx)
  (syntax-case stx ()
    [(_ status reason)
     (with-syntax ([http-XXX (format-id stx "http-~a" (syntax-e #'status))]
                   [-http-XXX (format-id stx "-http-~a" (syntax-e #'status))])
       #'(begin
           (struct http-XXX http-response () #:transparent
                   #:name -http-XXX #:constructor-name -http-XXX)
           (define (http-XXX [headers null] [in-port (open-input-bytes #"")])
             (-http-XXX headers in-port status reason 1.1))))]))

(define http/1.1-response-parser
  (http-response-parser http/1.1-header-value-decoders))

(define-http-response 100 "Continue")
(define-http-response 101 "Switching Protocols")
(define-http-response 200 "OK")
(define-http-response 201 "Created")
(define-http-response 202 "Accepted")
(define-http-response 203 "Non-Authoritative Information")
(define-http-response 204 "No Content")
(define-http-response 205 "Reset Content")
(define-http-response 206 "Partial Content")
(define-http-response 300 "Multiple Choices")
(define-http-response 301 "Moved Permanently")
(define-http-response 302 "Found")
(define-http-response 303 "See Other")
(define-http-response 304 "Not Modified")
(define-http-response 305 "Use Proxy")
(define-http-response 307 "Temporary Redirect")
(define-http-response 400 "Bad Request")
(define-http-response 401 "Unauthorized")
(define-http-response 402 "Payment Required")
(define-http-response 403 "Forbidden")
(define-http-response 404 "Not Found")
(define-http-response 405 "Method Not Allowed")
(define-http-response 406 "Not Acceptable")
(define-http-response 407 "Proxy Authentication Required")
(define-http-response 408 "Request Timeout")
(define-http-response 409 "Conflict")
(define-http-response 410 "Gone")
(define-http-response 411 "Length Required")
(define-http-response 412 "Precondition Failed")
(define-http-response 413 "Payload Too Large")
(define-http-response 414 "URI Too Long")
(define-http-response 415 "Unsupported Media Type")
(define-http-response 416 "Range Not Satisfiable")
(define-http-response 417 "Expectation Failed")
(define-http-response 426 "Upgrade Required")
(define-http-response 500 "Internal Server Error")
(define-http-response 501 "Not Implemented")
(define-http-response 502 "Bad Gateway")
(define-http-response 503 "Service Unavailable")
(define-http-response 504 "Gateway Timeout")
(define-http-response 505 "HTTP Version Not Supported")


;;; Unit Tests

(module+ test
  (require rackunit)

  (test-equal? "http-response-printer writes an http-response."
               (>>> http/1.1-response-printer
                    (http-response '([Content-Length . 10])
                                   (open-input-bytes #"123456789012345") 200 "OK" 1.1))
               (bytes-append #"HTTP/1.1 200 OK\r\n"
                             #"Content-Length: 10\r\n"
                             #"\r\n"
                             #"1234567890"))

  (test-case "http-response-parser reads an http-response."
             (let ([rep (<<< http/1.1-response-parser
                             (bytes-append #"HTTP/1.1 200 OK\r\n"
                                           #"host: localhost\r\n"
                                           #"content-length: 10\r\n"
                                           #"\r\n"
                                           #"123456789012345"))])
               (check-equal? (http-message-headers rep) `([Host . "localhost"]
                                                          [Content-Length . 10]))
               (check-equal? (http-message-body rep) "1234567890")
               (check = (http-response-status rep) 200)
               (check-equal? (http-response-reason rep) "OK")
               (check = (http-response-version rep) 1.1)))

  (test-exn "http-response-parser raises BAD-VERSION on bad version."
            (λ (err) (eq? err 'BAD-VERSION))
            (λ () (<<< http/1.1-response-parser #"JTTP/1.1 200 OK\r\n\r\n")))

  (test-exn "http-response-parser raises BAD-STATUS on bad status."
            (λ (err) (eq? err 'BAD-STATUS))
            (λ () (<<< http/1.1-response-parser #"HTTP/1.1 abc OK\r\n\r\n")))

  (test-exn "http-response-parser raises BAD-REASON on bad reason."
            (λ (err) (eq? err 'BAD-REASON))
            (λ () (<<< http/1.1-response-parser #"HTTP/1.1 200 \r\n\r\n"))))
