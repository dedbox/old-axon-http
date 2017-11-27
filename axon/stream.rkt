;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/process
         axon/syntax
         racket/generic)

(struct
    stream (sink source)
    #:property prop:evt (λ (s) (dead-evt s))
    #:property prop:procedure (case-lambda [(s) (s (void))]
                                           [(s msg) (send s msg) (recv s)])

    #:methods gen:process

    [(define/generic g-alive? alive?)
     (define/generic g-dead-evt dead-evt)
     (define/generic g-hangup hangup)
     (define/generic g-break break)
     (define/generic g-send send)
     (define/generic g-recv-evt recv-evt)
     (define/generic g-empty? empty?)
     (define/generic g-done? done?)
     (define/generic g-done-evt done-evt)

     (define (alive? s)
          (or (g-alive? (stream-sink s))
              (g-alive? (stream-source s))))

     (define (dead-evt s)
       (all-evts (g-dead-evt (stream-sink s))
                 (g-dead-evt (stream-source s))))

     (define (hangup s)
       (g-hangup (stream-sink s))
       (g-hangup (stream-source s)))

     (define (break s)
       (g-break (stream-sink s))
       (g-break (stream-source s)))

     (define (send s msg)
       ((stream-sink s) msg))

     (define (recv-evt s)
       (g-send (stream-source s) (void))
       (g-recv-evt (stream-source s)))

     (define (empty? s)
       (and (g-empty? (stream-sink s))
            (g-empty? (stream-source s))))

     (define (done? s)
       (and (g-done? (stream-source s))))

     (define (done-evt s)
       (all-evts (g-dead-evt (stream-sink s))
                 (g-done-evt (stream-source s))))])

;;; Unit Tests

(module+ test
  (require rackunit
           axon/queue
           axon/sink
           axon/source)

  (test-case
   "A stream is alive if its sink or source is alive."

   (test-true "A stream is alive if its sink is alive."
              (let ([s (stream (sink void) (source void))])
                (invariant (<== (alive? s) (alive? (stream-sink s)))
                  (kill (stream-sink s)))))

   (test-true "A stream is alive if its source is alive."
              (let ([s (stream (sink void) (source void))])
                (invariant (<== (alive? s) (alive? (stream-source s)))
                  (kill (stream-source s))))))

  (test-case
   "A stream is dead iff its sink and source are dead."

   (test-case
    "A stream is dead if its sink and source are dead."

    (test-true "A stream is dead if its sink>>source are dead."
               (let ([s (stream (sink void) (source void))])
                 (invariant (<== (dead? s) (and (dead? (stream-sink s))
                                                (dead? (stream-source s))))
                   (kill (stream-sink s))
                   (kill (stream-source s)))))

    (test-true "A stream is dead if its source>>sink are dead."
               (let ([s (stream (sink void) (source void))])
                 (invariant (<== (dead? s) (and (dead? (stream-sink s))
                                                (dead? (stream-source s))))
                   (kill (stream-source s))
                   (kill (stream-sink s))))))

   (test-true "A stream is dead only if its source and sink are dead."
              (let ([s (stream (sink void) (source void))])
                (invariant (<+> (alive? s) (and (dead? (stream-sink s))
                                                (dead? (stream-source s))))
                  (kill s))))

   (test-case "A stream sends to its sink."
              (let* ([k 0]
                     [s (stream (sink _ (set! k (add1 k))) (source void))])
                (for ([_ 10]) (send s 0))
                (check = 10 k)))

   (test-case "A stream recvs from its source."
              (let* ([k 0]
                     [s (stream (sink void) (source () (set! k (add1 k)) 0))])
                (for ([_ 10]) (recv s))
                (check = 10 k))))

  (test-true "A stream is empty if its source is empty."
             (let ([s (stream (sink void) (limit 5 (asource () 1)))])
               (invariant (<==> (empty? s) (empty? (stream-source s)))
                 (sleep 0.1)
                 (kill s)
                 (for ([_ 6]) (recv s)))))

  (test-true "A stream is done iff its sink is dead and its source is done."
             (let ([s (stream (sink void) (limit 5 (asource () 1)))])
               (invariant (<==> (done? s) (and (dead? (stream-sink s))
                                               (done? (stream-source s))))
                 (sleep 0.1)
                 (kill s)
                 (for ([_ 6]) (recv s))))))
