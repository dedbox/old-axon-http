;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/syntax)

(-struct source -filter ())

(define-syntax source
  (syntax-rules ()
    [(_ () block ... expr) (source (λ () block ... expr))]
    [(_ proc-def)
     (-source (spawn ()
                (let ([proc proc-def])
                  (ready)
                  (forever (take) (let ([msg (proc)]) (give msg))))))]))

(define-syntax asource
  (syntax-rules ()
    [(_ () block ... expr) (asource (λ () block ... expr))]
    [(_ proc-def)
     (-source (spawn ()
                (let ([proc proc-def])
                  (ready)
                  (forever (try-take) (let ([msg (proc)]) (give msg))))))]))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/process
           axon/queue)

  (test-case "A source runs only when triggered."
             (let ([f (source die)])
               (check-false (dead? f))
               (f)
               (sync f)
               (check-true (dead? f))))

  (test-case "An asource runs until its out-queue is full."
             (let* ([x 0]
                    [f (limit 5 (asource () (set! x (add1 x)) x))])
               (check-true (alive? f))
               (sleep 0.1)
               (check = 6 x))))
