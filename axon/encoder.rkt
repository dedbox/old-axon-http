;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/sink
         axon/syntax)

(-struct encoder -sink (out-port))

(define ((encoder printer) out-port)
  (let ([f (spawn ()
             (with-handlers ([exn:break:terminate? void])
               (with-handlers ([exn:fail? void]
                               [exn:break:hang-up? void])
                 (ready)
                 (forever
                   (let ([msg (sync (port-closed-evt out-port) (take-evt))])
                     (when (or (port-closed? out-port) (eof? msg))
                       (quit))
                     (printer msg out-port)
                     (give))))
               (close-output-port out-port)))])
    (-encoder f out-port)))

(define (encode out-port printer)
  ((encoder printer) out-port))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/process)

  (test-case "encode serializes messages to its out-port."
             (let ([f (encode (open-output-string) displayln)])
               (f 1)
               (f "A")
               (check equal? "1\nA\n"
                     (get-output-string (encoder-out-port f)))))

  (test-true "encode is dead if its out-port is closed."
             (let* ([out-port (open-output-string)]
                    [f (encode out-port displayln)])
               (invariant (<== (dead? f) (port-closed? out-port))
                 (ibegin (close-output-port out-port) (sync f)))))

  (test-true "encode closes its out-port when stopped."
             (let* ([out-port (open-output-string)]
                    [f (encode out-port displayln)])
               (stop f)
               (port-closed? out-port)))

  (test-false "encode does not close its out-port when killed."
              (let* ([out-port (open-output-string)]
                    [f (encode out-port displayln)])
               (kill f)
               (port-closed? out-port))))
