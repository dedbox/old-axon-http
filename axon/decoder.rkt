;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/process
         axon/queue
         axon/source
         axon/syntax)

(-struct decoder -source (in-port))

(define ((decoder parser) in-port)
  (let* ([parse (serve _ (if (port-closed? in-port) eof (parser in-port)))]
         [parse-evt (recv-evt parse)]
         [f (spawn ()
              (with-handlers ([exn:break:terminate? void])
                   (with-handlers ([exn:fail? void]
                                   [exn:break:hang-up? void])
                     (ready)
                     (forever
                       (take)
                       (send parse (void))
                       (let ([msg (sync (port-closed-evt in-port) parse-evt)])
                         (if (or (port-closed? in-port) (eof? msg))
                             (quit)
                             (give msg)))))
                   (close-input-port in-port)))])
    (-decoder f in-port)))

(define (decode in-port parser)
  ((decoder parser) in-port))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case "decode deserializes messages from its in-port."
             (let ([f (decode (open-input-string "1\nA\n") read)])
               (check = 1 (f))
               (check eq? 'A (f))
               (check-pred eof? (f))))

  (test-true "decode is dead if its in-port is closed."
             (let* ([in-port (open-input-string "1\nA\n")]
                    [f (decode in-port read)])
               (invariant (<== (dead? f) (port-closed? in-port))
                 (ibegin (close-input-port in-port) (f) (sync f)))))

  (test-true "decode closes its in-port when stopped."
             (let* ([in-port (open-input-string "1\nA\n")]
                    [f (decode in-port read)])
               (stop f)
               (port-closed? in-port)))

  (test-false "decode does not close its in-port when killed."
              (let* ([in-port (open-input-string "1\nA\n")]
                     [f (decode in-port read)])
                (kill f)
                (port-closed? in-port))))
