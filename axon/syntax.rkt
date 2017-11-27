;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/port)

(require [for-syntax racket/base
                     racket/syntax])

(define-syntax-rule (require/provide mod ...)
  (begin (require mod ...)
         (provide [all-from-out mod ...])))

(define-syntax bind
  (syntax-rules ()
    [(_ qs)    (bind qs () eof)]
    [(_ qs ps) (bind qs ps eof)]
    [(_ ([q expr1-0 expr1 ...] ...)
        ([p expr2-0 expr2 ...] ...)
        default)
     (match-lambda [`q expr1-0 expr1 ...] ...
                   [ p expr2-0 expr2 ...] ...
                   [ _ default])]))

(define-syntax-rule (forever block ... stmt)
  (let loop () block ... stmt (loop)))

(define-syntax-rule (until test block ... stmt)
  (let loop () (unless test block ... stmt (loop))))

(define eof? eof-object?)

(define (all-evts . es)
  (if (null? es)
      (handle-evt always-evt (λ _ #t))
      (replace-evt (apply choice-evt (map (λ (e) (handle-evt e (λ _ e))) es))
                   (λ (e) (apply all-evts (remq e es))))))

(define (syncs? evt [t 0])
  (and (sync/timeout t evt) #t))

(define (blocks? evt [t 0])
  (not (syncs? evt t)))

(define (==> P Q)
  (or (not P) (and Q #t)))

(define (<== P Q)
  (==> Q P))

(define (<==> P Q)
  (and (==> P Q) (<== P Q)))

(define (<+> P Q)
  (and (or P Q) (not (and P Q))))

(define-syntax invariant
  (syntax-rules (ibegin for let)
    [(_ chk)  chk]

    [(_ (ibegin body ...)) (begin body ...)]

    [(_ chk (for clauses body ... expr))
     (and (for clauses (invariant chk body ... expr)) #t)]

    [(_ chk (let bindings body ... expr))
     (and (let bindings (invariant chk body ... expr)) #t)]

    [(_ chk stmt block ...)
     (and chk (begin stmt (invariant chk block ...)))]))

(define-syntax-rule (ibegin body ...)
  (begin body ...))

(define-syntax (-struct stx)
  (syntax-case stx ()
   [(_ name args ...)
    (with-syntax ([-name (format-id stx "-~a" #'name)])
      #'(struct name args ... #:name -name #:constructor-name -name))]))

(define ((flushing printer) msg out-port)
  (printer msg out-port)
  (flush-output out-port))

(define o compose1)
(define string->bytes string->bytes/utf-8)
(define symbol->bytes (o string->bytes symbol->string))
(define number->bytes (o string->bytes number->string))

(define (copy-bytes len in-port out-port)
  (cond [(not len) (copy-port in-port out-port)]
        [(> len 0) (let* ([N (min len 4096)]
                          [bstr (read-bytes N in-port)])
                     (unless (eof? bstr)
                       (write-bytes bstr out-port)
                       (copy-bytes (- len N) in-port out-port)))]))

;;; Unit Tests

(module+ test
  (require rackunit
           racket/list)

  (test-true "Either all-evts blocks or all of its events sync."
             (let* ([I (shuffle (for/list ([i 10]) i))]
                    [ts (map (λ _ (thread (λ () (thread-receive)))) I)]
                    [all-ts (apply all-evts ts)])
               (invariant (<+> (blocks? all-ts) (andmap syncs? ts))
                 (for ([i 10])
                   (let ([t (list-ref ts i)])
                     (thread-send t -1)
                     (thread-wait t)))))))
