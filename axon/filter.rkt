;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/lock
         axon/process
         axon/queue
         axon/syntax
         racket/generic)

(struct
    filter (thread lock out-queue)
    #:property prop:evt (λ (f) (dead-evt f))
    #:property prop:procedure (case-lambda [(f) (f (void))]
                                           [(f msg) (send f msg) (recv f)])

    #:methods gen:process

    [(define (alive? f)
       (thread-running? (filter-thread f)))

     (define (dead-evt f)
       (thread-dead-evt (filter-thread f)))

     (define (hangup f)
       (break-thread (filter-thread f) 'hang-up))

     (define (break f)
       (break-thread (filter-thread f) 'terminate))

     (define (send f [msg (void)])
       (when (alive? f)
         (thread-send (filter-thread f) msg))
       (sleep 0))

     (define (recv-evt f)
       (make-recv-evt f))

     (define (empty? f)
       (queue-empty? (filter-out-queue f)))

     (define (done? f)
       (unlocked? (filter-lock f)))

     (define (done-evt f)
       (unlock-evt (filter-lock f)))])

(define (make-recv-evt f)
  (choice-evt
   (handle-evt (done-evt f) (λ _ eof))
   (handle-evt (filter-out-queue f)
               (λ (msg)
                 (when (and (dead? f) (empty? f))
                   (unlock (filter-lock f)))
                 msg))))

(define current-ready-latch (make-parameter #f))

(define-syntax-rule (ready)
  (semaphore-post (current-ready-latch)))

(define current-out-queue (make-parameter #f))

(define-syntax spawn
  (syntax-rules ()
    [(_ () block ... stmt) (spawn (λ () block ... stmt))]
    [(_ proc-def)
     (let* ([ready-latch (make-semaphore 0)]
            [L (lock)]
            [Q (queue)]
            [t (thread (λ ()
                         (parameterize ([current-ready-latch ready-latch]
                                        [current-out-queue Q])
                           (with-handlers ([exn:break? void])
                             (let ([proc proc-def])
                               (proc)))
                           (ready)
                           (when (queue-empty? Q)
                             (unlock L)))))])
       (semaphore-wait ready-latch)
       (filter t L Q))]))

(define-syntax run
  (syntax-rules ()
    [(_ () block ... stmt) (run (λ () block ... stmt))]
    [(_ proc-def) (spawn () (let ([proc proc-def]) (ready) (proc)))]))

(define (quit . _)
  (break-thread (current-thread) 'hang-up))

(define (die . _)
  (break-thread (current-thread) 'terminate))

(define (deadlock . _)
  (thread-wait (current-thread)))

(define give
  (case-lambda
    [() (give (void))]
    [(msg) (queue-put (current-out-queue) msg) (sleep 0)]))

(define (take)
  (sync (take-evt)))

(define (take-evt)
  (handle-evt (thread-receive-evt)
              (λ _ (let ([msg (thread-receive)])
                     (if (eof? msg) (quit) msg)))))

(define (try-take)
  (thread-try-receive))

(define-syntax-rule (async () block ... expr)
  (recv-evt (limit 1 (spawn ()
                       (with-handlers ([exn:fail? (λ _ eof)])
                         (ready) block ... (give expr))))))

(define-syntax serve
  (syntax-rules ()
    [(_ arg blk ... exp) (serve (λ arg blk ... exp))]
    [(_ proc-def) (spawn ()
                    (let ([proc proc-def])
                      (ready)
                      (forever (give (proc (take))))))]))

(define (pipe . fs)
  (serve (msg) (foldl (λ (f msg*) (f msg*)) msg fs)))

(define (couple f1 f2)
  (spawn ()
    (define evts (list (handle-evt (recv-evt f1) (λ (msg) (send f2 msg)))
                       (handle-evt (recv-evt f2) (λ (msg) (send f1 msg)))))
    (ready)
    (forever (apply sync evts))))

(struct
    -filter (f)
    #:property prop:evt (λ (b) (dead-evt b))
    #:property prop:procedure (case-lambda [(b) (b (void))]
                                           [(b msg) (send b msg) (recv b)])

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

     (define (alive? b)
       (g-alive? (-filter-f b)))

     (define (dead-evt b)
       (g-dead-evt (-filter-f b)))

     (define (hangup b)
       (g-hangup (-filter-f b)))

     (define (break b)
       (g-break (-filter-f b)))

     (define (send b msg)
       (g-send (-filter-f b) msg))

     (define (recv-evt b)
       (g-recv-evt (-filter-f b)))

     (define (empty? b)
       (g-empty? (-filter-f b)))

     (define (done? b)
       (g-done? (-filter-f b)))

     (define (done-evt b)
       (g-done-evt (-filter-f b)))])

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-true "A fresh filter is alive." (alive? (run deadlock)))

  (test-false "A fresh filter is not dead." (dead? (run deadlock)))

  (test-true "quit makes a filter dead."
             (let ([f (run quit)]) (sync f) (dead? f)))

  (test-true "die makes a filter dead."
             (let ([f (run die)]) (sync f) (dead? f)))

  (test-true "dead-evt syncs when its filter is dead."
             (and (sync (dead-evt (run die))) #t))

  (test-true "kill makes a filter dead."
             (let ([f (run deadlock)]) (kill f) (dead? f)))

  (test-true "A filter syncs if it is dead."
             (let ([f (run deadlock)])
               (invariant (==> (syncs? f) (dead? f))
                 (kill f))))

  (test-true "A filter is either alive or dead."
             (let ([f (run deadlock)])
               (invariant (<+> (alive? f) (dead? f))
                 (kill f))))

  (test-case "A filter can send and receive messages."
             (let ([f (run () (forever (give (> (take) 0))))])
               (check-true (f 1))
               (check-false (f 0))))

  (test-true "A filter can fill its out-queue."
             (let ([f (limit 5 (run () (for ([_ 5]) (give 1))))])
               (until (syncs? f) #t)
               (queue-full? (filter-out-queue f))))

  (test-true "A filter is empty if its out-queue is empty."
             (let ([f (run deadlock)])
               (invariant (<== (empty? f) (queue-empty? (filter-out-queue f)))
                 (kill f))))

  (test-case
   "A filter is done iff it is dead and empty."

   (test-true "A filter is done if it is dead and empty."
              (let ([f (run () (give 1))])
                (invariant (<==> (done? f) (and (dead? f) (empty? f)))
                  (kill f))))

   (test-true "A filter is done only if it is dead and empty."
              (let ([f (run () (give) (die))])
                (invariant (<==> (done? f) (and (dead? f) (empty? f)))
                  (recv f)
                  (sync f)))))

  (test-true "A dead filter emits eof to outstanding recv-evts."
             (let* ([f (run deadlock)]
                    [es (for/list ([_ 10]) (recv-evt f))]
                    [all-es (apply all-evts es)])
               (invariant (<+> (syncs? all-es) (ormap blocks? es))
                 (kill f))))

  (test-true "A pipe is a one-way series of filters."
             (= 5 ((pipe add1 add1 add1) 2)))

  (test-case "A couple is a bi-directional pair of filters."
             (couple (serve add1)
                     (run () (give 1) (check = 2 (take))))
             (void)))
