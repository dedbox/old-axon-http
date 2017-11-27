;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/syntax)

(-struct sink -filter ())

(define-syntax sink
  (syntax-rules ()
    [(_ arg block ... stmt) (sink (λ arg block ... stmt))]
    [(_ proc-def)
     (-sink (spawn ()
              (let ([proc proc-def])
                (ready)
                (forever (let ([msg (take)]) (proc msg) (give))))))]))

(define-syntax asink
  (syntax-rules ()
    [(_ arg block ... stmt) (asink (λ arg block ... stmt))]
    [(_ proc-def)
     (-sink (spawn ()
              (let ([proc proc-def])
                (ready)
                (forever (let ([msg (take)]) (give) (proc msg))))))]))

;;; Unit Tests

(module+ test
  (require rackunit
           axon/process)

  (test-case "A sink runs only when triggered."
             (let ([f (sink die)])
               (check-false (dead? f))
               (f)
               (check-true (and (sync f) #t))))

  (test-true "A sink responds to every message with void."
             (let ([f (sink _ 1)])
               (andmap void? (for/list ([_ 10]) (f))))))
