;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/codec
         axon/filter
         axon/process
         axon/source
         axon/syntax
         racket/tcp)

(define (tcp-client cdc [port-no 3600] [hostname "localhost"])
  (call-with-values (λ () (tcp-connect hostname port-no)) cdc))

(define (tcp-server cdc [port-no 3600] [hostname #f])
  (-source (spawn ()
             (let ([listener (tcp-listen port-no 4 #t hostname)])
               (with-handlers ([exn:break? void])
                 (ready)
                 (forever
                   (give (call-with-values (λ () (tcp-accept listener)) cdc))))
               (tcp-close listener)))))

(define (tcp-service cdc responder [port-no 3600] [hostname #f])
  (spawn ()
    (define srv (tcp-server cdc port-no hostname))
    (define peers (make-hash))

    (define (accept sock)
      (let ([addr (call-with-values
                      (λ () (tcp-addresses (codec-in-port sock) #t)) list)])
        (hash-set! peers addr
                   (spawn ()
                     (let* ([f (responder)]
                            [c (couple f sock)])
                       (with-handlers ([exn:break:terminate? void])
                         (with-handlers ([exn:break:hang-up? void])
                           (ready)
                           (sync f))
                         (stop c))
                       (hash-remove! peers addr))))))

    (define control
      (bind ([clients (hash-keys peers)]
             [(stop ,addr) (if (hash-has-key? peers addr)
                               (begin (stop (hash-ref peers addr)) `(stopped ,addr))
                               'not-found)])
            () 'bad-command))

    (with-handlers ([exn:break:hang-up? void])
      (ready)
      (send srv (void))
      (forever
        (sync (handle-evt (recv-evt srv) accept)
              (handle-evt (take-evt) (λ (msg)
                                       (let ([rep (control msg)]) (give rep)))))))
    (stop srv)
    (apply stop-all (hash-values peers))))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/format)

  (let* ([srv (tcp-server sexp-codec)]
         [cli (tcp-client sexp-codec)]
         [peer (srv)])
    (send cli 1)
    (check = 1 (recv peer))

    (send peer 'A)
    (check eq? 'A (recv cli))

    (send peer eof)
    (check-pred eof? (recv cli))

    (send cli eof)
    (check-pred eof? (recv peer))

    (check-true (and (sync (all-evts peer cli)) #t))
    (stop srv))

  (let ([svc (tcp-service sexp-codec (λ () (serve (x) x)))])
    (sync svc)))
