;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/syntax)

(require [for-syntax racket/base syntax/parse])

(define bytes->string bytes->string/utf-8)
(define bytes->symbol (o string->symbol bytes->string))
(define bytes->number (o string->number bytes->string))

(define bytes->string/titlecase (o string-titlecase bytes->string))
(define bytes->symbol/titlecase (o string->symbol bytes->string/titlecase))

(define current-syntax-error (make-parameter (λ _ 'SYNTAX-ERROR)))

(define (!! parser err-maker)
  (λ (in-port)
    (parameterize ([current-syntax-error err-maker])
      (parser in-port))))

(define (TOK pattern)
  (λ (in-port)
    (let ([matches (regexp-match pattern in-port)])
      (cond [(not matches) (raise ((current-syntax-error)))]
            [(null? (cdr matches)) #t]
            [(null? (cddr matches)) (cadr matches)]
            [else (cdr matches)]))))

(define ((<* r0 . rs) arg)
  (begin0 (r0 arg) (for-each (λ (r) (r arg)) rs)))

(define ((*> r0 . rs) arg)
  (if (null? rs) (r0 arg) (begin (r0 arg) ((apply *> rs) arg))))

(define ((>>= e f) arg)
  (f (e arg)))

(define ((=<< f e) arg)
  (f (e arg)))

(define (<<< parser bstr)
  (parser (open-input-bytes bstr)))

(define (>>> printer msg)
  (let ([out-port (open-output-bytes)])
    (printer msg out-port)
    (get-output-bytes out-port)))

(define (STOK pattern) (>>= (TOK pattern) bytes->string))

(define SP (TOK #" "))
(define CRLF (TOK #"\r\n"))
(define NUM (>>= (TOK #px#"^(-?\\d+(?:\\.\\d+)?)") bytes->number))
(define SYM (>>= (TOK #px#"^([^ \t\r\n]+)") bytes->symbol))

(define-syntax (parser stx)
  (syntax-parse stx #:datum-literals (:= ?? <- !!)
    [(_ [var:id := def] blk:expr ...+) #'(let ([var def]) (parser blk ...))]
    [(_ [?? test:expr] blk:expr ...+) #'(parser [?? test !! 'SYNTAX-ERROR] blk ...)]
    [(_ [?? test:expr !! err:expr] blk ...+) #'(if test (parser blk ...) (raise err))]
    [(_ [<- e:expr] blk:expr ...+) #'(parser [<- e !! 'SYNTAX-ERROR] blk ...)]
    [(_ [<- e:expr !! err:expr] blk ...+)
     #'(λ (in-port)
         (parameterize ([current-syntax-error (λ _ err)])
           (e in-port))
         ((parser blk ...) in-port))]
    [(_ [var:id <- e:expr] blk:expr ...+) #'(parser [var <- e !! 'SYNTAX-ERROR] blk ...)]
    [(_ [var:id <- e:expr !! err:expr] blk:expr ...+)
     #'(λ (in-port)
         (let ([var (parameterize ([current-syntax-error (λ _ err)])
                      (e in-port))])
           ((parser blk ...) in-port)))]
    [(_ e:expr) #'(λ _ e)]))

(define-syntax-rule (parse in-port blk ...)
  ((parser blk ...) in-port))

;;; Unit Tests

(module+ test
  (require rackunit)

  (check equal? (parse (open-input-bytes #"3  4 A  5")
                       [x <- (<* NUM SP)]
                       [y <- (*> SP NUM)]
                       [z <- (>>= (*> SP (<* SYM SP)) symbol->string)]
                       [w <- (=<< sub1 (*> SP NUM))]
                       (list (+ x y) z w))
         '(7 "A" 4))

  (check = (parse (open-input-bytes #"6 ")
                  [x := 5]
                  [y <- NUM]
                  [<- SP]
                  [?? #t !! #f]
                  (+ x y))
         11)

  (check-exn (λ (e) (eq? 'SYNTAX-ERROR e))
             (λ () (parse (open-input-bytes #" ")
                          [a <- SP]
                          [b <- SP]
                          (list a b)))))
