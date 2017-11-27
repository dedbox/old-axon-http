;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/format
         axon/syntax
         json)

(define-format json read-json (flushing write-json))
