;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(define current-limit (make-parameter 50))

(define-syntax-rule (limit N block ... stmt)
  (parameterize ([current-limit N])
    block ... stmt))

(struct queue (limit in in-length out out-length not-empty not-full)
        #:transparent
        #:mutable
        #:name queue*
        #:constructor-name queue*
        #:property prop:evt (λ (Q) (queue-take-evt Q)))

(define (queue [L (current-limit)])
  (queue* L null 0 null 0 (make-semaphore 0)
          (if L (make-semaphore L) always-evt)))

(define (queue-length Q)
  (+ (queue-in-length Q)
     (queue-out-length Q)))

(define (queue-empty? Q)
  (= 0 (queue-length Q)))

(define (queue-full? Q)
  (= (queue-length Q)
     (or (queue-limit Q) -1)))

(define (queue-rebalance Q)
  (when (or (null? (queue-out Q))
            (> (queue-in-length Q)
               (queue-out-length Q)))
    (set-queue-out! Q (append (queue-out Q) (reverse (queue-in Q))))
    (set-queue-out-length! Q (+ (queue-out-length Q)
                                (queue-in-length Q)))
    (set-queue-in! Q null)
    (set-queue-in-length! Q 0)))

(define (queue-put Q x)
  (sync (queue-put-evt Q x)))

(define (queue-put-evt Q x)
  (handle-evt (if (queue-limit Q) (queue-not-full Q) always-evt)
              (λ _
                (set-queue-in! Q (cons x (queue-in Q)))
                (set-queue-in-length! Q (add1 (queue-in-length Q)))
                (queue-rebalance Q)
                (when (queue-limit Q)
                  (semaphore-post (queue-not-empty Q))))))

(define (queue-take Q)
  (sync (queue-take-evt Q)))

(define (queue-take-evt Q)
  (handle-evt (queue-not-empty Q)
              (λ _
                (begin0
                    (car (queue-out Q))
                  (set-queue-out! Q (cdr (queue-out Q)))
                  (set-queue-out-length! Q (sub1 (queue-out-length Q)))
                  (queue-rebalance Q)
                  (when (semaphore? (queue-not-full Q))
                    (semaphore-post (queue-not-full Q)))))))

;; BUG old semaphores are stale after queue-clear (use guard-evt?)
(define (queue-clear Q)
  (set-queue-in! Q null)
  (set-queue-in-length! Q 0)
  (set-queue-out! Q null)
  (set-queue-out-length! Q 0)
  (set-queue-not-empty! Q (make-semaphore 0))
  (set-queue-not-full!
   Q (if (queue-limit Q) (make-semaphore (queue-limit Q)) always-evt)))

(module+ test
  (require rackunit
           axon/syntax)

  (test-true "A fresh queue is empty." (queue-empty? (queue)))
  (test-true "A fresh queue has zero length." (= 0 (queue-length (queue))))

  (test-case
   "queue-put increments the length of its queue."
   (let ([Q (queue)])
     (check-false (memq #f (for/list ([i 10])
                             (queue-put Q -1)
                             (= (add1 i) (queue-length Q)))))))

  (test-case
   "queue-take decrements the length of its queue."
   (let ([Q (queue)])
     (for ([_ 10]) (queue-put Q -1))
     (check-false (memq #f (for/list ([i 10])
                             (queue-take Q)
                             (= (- 9 i) (queue-length Q)))))))

  (test-case
   "Either a queue is empty or its length is positive."
   (let ([Q (queue)])
     (check-true (invariant (or (queue-empty? Q) (> (queue-length Q) 0))
                   (queue-put Q -1)
                   (queue-take Q)))))

  (test-case
   "Either queue-put-evt syncs or its queue is full."
   (let ([Q (limit 5 (queue))])
     (check-true (invariant (<+> (queue-full? Q) (syncs? (queue-put-evt Q -1)))
                   0 0 0 0 0 0 0 0 0 0))))

  (test-case
   "Either queue-take-evt syncs or its queue is empty."
   (let ([Q (queue)])
     (check-true (invariant (<+> (queue-empty? Q) (syncs? (queue-take-evt Q)))
                   (queue-put Q -1)
                   (for ([_ 5]) (queue-put Q -1))
                   0 0 0 0 0 0 0 0 0 0))))

  (test-case
   "A queue is full iff its length is at its limit."
   (let ([Q (limit 3 (queue))])
     (check-true (invariant
                     (<==> (queue-full? Q) (= (queue-length Q) (queue-limit Q)))
                   (queue-put Q -1) (queue-put Q -1) (queue-put Q -1)
                   (queue-take Q) (queue-take Q) (queue-take Q)))))

  (test-case
   "A queue with no limit is never full."
   (let ([Q (queue #f)])
     (check-true (invariant (not (queue-full? Q))
                   (for ([_ 1000])
                     (queue-put Q -1))))))

  (test-case
   "queue-clear empties a queue."

   (test-case "queue-clear empties a limited queue."
              (let ([Q (queue)])
                (for ([_ 10]) (queue-put Q -1))
                (queue-clear Q)
                (check-pred queue-empty? Q)
                (check-true (= 0 (queue-length Q)))))

   (test-case "queue-clear empties an unlimited queue."
              (let ([Q (queue #f)])
                (for ([_ 10]) (queue-put Q -1))
                (queue-clear Q)
                (check-pred queue-empty? Q)
                (check-true (= 0 (queue-length Q)))))))
