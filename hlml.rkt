#lang racket

(require "prototype.rkt")
(require racket/cmdline)

(define html-compile (make-parameter #f))


(define read-file
  (command-line
   #:program "hlml-evaluator"
   #:once-any
   [("-o" "--html") "Compile HLML -> HTML" (html-compile #t)]
   #:args (filename)
   (let [(hlml (eval-hlml (open-input-file filename)))]
     (display (if html-compile (->html hlml) hlml)))))
