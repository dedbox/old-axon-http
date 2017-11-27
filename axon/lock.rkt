;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(struct lock (thread)
        #:name lock*
        #:constructor-name lock*
        #:property prop:evt (λ (L) (unlock-evt L)))

(define (lock)
  (let ([ready (make-semaphore 0)])
    (begin0
        (lock* (thread (λ () (semaphore-post ready) (thread-receive))))
      (semaphore-wait ready))))

(define (locked? L)
  (thread-running? (lock-thread L)))

(define (unlocked? L)
  (thread-dead? (lock-thread L)))

(define (unlock L)
  (when (locked? L)
    (thread-send (lock-thread L) (void))
    (thread-wait (lock-thread L))))

(define (unlock-evt L)
  (if (unlocked? L)
      (handle-evt always-evt (λ _ #t))
      (handle-evt (thread-dead-evt (lock-thread L)) (λ _ #t))))

(module+ test
  (require rackunit
           racket/list
           axon/syntax)

  (define (u-lock)
    (let ([L (lock)]) (unlock L) L))

  (test-true "A fresh lock is locked." (locked? (lock)))
  (test-true "A fresh lock is not unlocked." (not (unlocked? (lock))))
  (test-true "unlock makes a lock unlocked." (unlocked? (u-lock)))

  (test-case
   "A lock stays locked until it is unlocked."
   (let ([L (lock)])
     (check-true (locked? L))
     (unlock L)
     (check-true (not (locked? L)))))

  (test-case
   "unlock is idempotent."
   (let ([L (lock)])
     (check-false (memq #f (for/list ([_ 1000]) (unlock L) (unlocked? L))))))

  (test-case
   "A lock is either locked or unlocked."
   (let ([L (lock)])
     (check-true (invariant (<+> (locked? L) (unlocked? L))
                   (unlock L)))))

  (test-case
   "Either a lock is unlocked or its unlock-evt blocks."
   (let* ([L (lock)]
          [e (unlock-evt L)])
     (check-true (invariant (<+> (unlocked? L) (blocks? e))))))

  (test-case
   "Either a lock is locked or its unlock-evt syncs."
   (let* ([L (lock)]
          [e (unlock-evt L)])
     (check-true (invariant (<+> (locked? L) (syncs? e))
                   (unlock L)))))

  (test-case "unlock-evts for an unlocked lock always sync."
             (let ([L (u-lock)])
               (check-true (andmap sync (for/list ([_ 10]) L))))))
