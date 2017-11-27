;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/encoder
         axon/decoder
         axon/stream
         axon/syntax)

(-struct codec stream (in-port out-port))

(define ((codec parser printer) in-port out-port)
  (-codec (encode out-port printer) (decode in-port parser) in-port out-port))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/process
           axon/queue
           axon/syntax)

  (test-true "codec serializes messages to its out-port."
             (let* ([in-port (open-input-string "")]
                    [out-port (open-output-string)]
                    [s ((codec read displayln) in-port out-port)])
               (send s 1)
               (send s "A")
               (equal? "1\nA\n" (get-output-string out-port))))

  (test-case "codec deserializes messages from its in-port."
             (let* ([in-port (open-input-string "1\nA\n")]
                    [out-port (open-output-string)]
                    [s ((codec read displayln) in-port out-port)])
               (check = 1 (recv s))
               (check eq? 'A (recv s))
               (check-pred eof? (recv s))))

  (test-case
   "codec dies when its in-port and out-port are closed."

   (test-true "codec dies when its in-port >> out-port are closed."
              (let* ([in-port (open-input-string "1\nA\n")]
                     [out-port (open-output-string)]
                     [s ((codec read displayln) in-port out-port)])
                (invariant (<== (dead? s) (and (port-closed? in-port)
                                               (port-closed? out-port)))
                  (close-input-port in-port)
                  (ibegin (close-output-port out-port) (s) (sync s)))))

   (test-true "codec dies when its out-port >> in-port are closed."
              (let* ([in-port (open-input-string "1\nA\n")]
                     [out-port (open-output-string)]
                     [s ((codec read displayln) in-port out-port)])
                (invariant (<== (dead? s) (and (port-closed? in-port)
                                               (port-closed? out-port)))
                  (close-output-port out-port)
                  (ibegin (close-input-port in-port) (s) (sync s))))))

  (test-true "codec closes its ports when stopped."
             (let* ([in-port (open-input-string "1\nA\n")]
                    [out-port (open-output-string)]
                    [s ((codec read displayln) in-port out-port)])
               (stop s)
               (and (port-closed? in-port) (port-closed? out-port))))

  (test-true "codec does not close its ports when killed."
             (let* ([in-port (open-input-string "1\nA\n")]
                    [out-port (open-output-string)]
                    [s ((codec read displayln) in-port out-port)])
               (stop s)
               (and (port-closed? in-port) (port-closed? out-port)))))
