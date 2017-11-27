;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/format
         axon/http/request
         axon/http/response
         axon/tcp)

(define-format http-server http/1.1-request-parser http/1.1-response-printer)

(define (http-server responder [port-no 3600] [hostname #f])
  (tcp-server http-server-codec port-no hostname))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/http/message
           axon/process)

  (test-case "http-server-codec reads requests"
             (let* ([in-port (open-input-bytes
                              (bytes-append #"PUT / HTTP/1.1\r\n"
                                            #"Content-Length: 4\r\n"
                                            #"\r\n"
                                            #"1234567"))]
                    [sock (http-server-codec in-port (open-output-bytes))]
                    [msg (recv sock)])
               (check equal? (http-message-headers msg) '([Content-Length . 4]))
               (check equal? (http-message-body msg) "1234")
               (check eq? (http-request-method msg) 'PUT)
               (check equal? (http-request-target msg) "/")
               (check = (http-request-version msg) 1.1)))

  (test-equal?
   "http-server-codec writes responses"
   (let-values ([(in-port _) (make-pipe)])
     (let* ([out-port (open-output-bytes)]
            [sock (http-server-codec in-port out-port)])
       (send sock (http-404 '([Content-Length . 5]) (open-input-bytes #"12345")))
       (send sock (http-200))
       (get-output-bytes out-port)))
   (bytes-append #"HTTP/1.1 404 Not Found\r\n"
                 #"Content-Length: 5\r\n"
                 #"\r\n"
                 #"12345"
                 #"HTTP/1.1 200 OK\r\n"
                 #"\r\n")))
