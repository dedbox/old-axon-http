;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/format
         axon/http/request
         axon/http/response
         axon/syntax
         axon/tcp)

(define-format http-client http/1.1-response-parser (flushing http/1.1-request-printer))

(define (http-client requester [port-no 3600] [hostname "localhost"])
  (couple requester (tcp-client http-client-codec port-no hostname)))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/filter
           axon/http/message
           axon/process)

  (test-equal?
   "http-client-codec writes requests"
   (let-values ([(in-port _) (make-pipe)])
     (let* ([out-port (open-output-bytes)]
            [sock (http-client-codec in-port out-port)])
       (send sock (http-PUT "/" '([Content-Length . 3]) (open-input-bytes #"123")))
       (send sock (http-GET "/"))
       (get-output-bytes out-port)))
   (bytes-append #"PUT / HTTP/1.1\r\n"
                 #"Content-Length: 3\r\n"
                 #"\r\n"
                 #"123"
                 #"GET / HTTP/1.1\r\n"
                 #"\r\n"))

  (test-case
   "http-client-codec reads responses"
   (let* ([in-port (open-input-bytes (bytes-append #"HTTP/1.1 200 OK\r\n"
                                                   #"Content-Length: 7\r\n"
                                                   #"\r\n"
                                                   #"1234567"))]
          [sock (http-client-codec in-port (open-output-bytes))]
          [msg (recv sock)])
     (check equal? (http-message-headers msg) '([Content-Length . 7]))
     (check equal? (http-message-body msg) "1234567")
     (check = (http-response-status msg) 200)
     (check equal? (http-response-reason msg) "OK")
     (check = (http-response-version msg) 1.1)))

  (let* ([req (http-PUT "/" '([Content-Length . 3]) (open-input-bytes #"123"))]
         [rep (http-200 '([Content-Length . 7]) (open-input-bytes #"1234567"))]
         [srv (serve (msg) (and (eq? msg req) rep))]
         [cli (run ()
                (give req)
                (check eq? (take) rep)
                (give 123)
                (check-false (take)))])
    (couple cli srv)
    (wait cli)
    (stop srv))

  ;; (let* ([f (run ()
  ;;             (give (http-GET "/"))
  ;;             (let ([msg (take)])
  ;;               (printf "\n  MSG ~v\n\n  BODY ~v\n" msg (http-message-body msg))))]
  ;;        [c (http-client f)])
  ;;   (sync f)
  ;;   (stop c))

  )
