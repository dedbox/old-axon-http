;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/syntax
         racket/generic)

(define-generics process
  (alive? process)
  (dead-evt process)
  (hangup process)
  (break process)
  (send process msg)
  (recv-evt process)
  (empty? process)
  (done? process)
  (done-evt process)

  ;; optional
  (dead? process)
  (wait process)
  (kill process)
  (stop process)
  (recv process)

  #:fallbacks
  [(define/generic g-alive? alive?)
   (define/generic g-dead-evt dead-evt)
   (define/generic g-hangup hangup)
   (define/generic g-break break)
   (define/generic g-wait wait)
   (define/generic g-recv-evt recv-evt)

   (define (dead? π)
     (not (g-alive? π)))

   (define (wait π)
     (sync (g-dead-evt π))
     (void))

   (define (kill π)
     (g-break π)
     (g-wait π))

   (define (stop π)
     (g-hangup π)
     (g-wait π))

   (define (recv π)
     (sync (g-recv-evt π)))])

(define (wait-all . πs)
  (sync (apply all-evts (map dead-evt πs)))
  (void))

(define (kill-all . πs)
  (map break πs)
  (apply wait-all πs))

(define (stop-all . πs)
  (map hangup πs)
  (apply wait-all πs))

(define (recv-all . πs)
  (map recv πs))
