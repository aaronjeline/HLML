#lang racket

(provide eval-hlml ->html)

(define-syntax-rule (.. f g) (λ (x) (g (f x))))

(define (make-unquote ast)
  (match ast
    [(cons _ _) (map make-unquote ast)]
    [s #:when (eq? 'script s) 'unquote]
    [o o]))

(define (pp ast)
  (cons 'quasiquote (list (make-unquote ast))))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (eval-hlml p)
  (eval (pp (read p)) ns))



(define (->html markup)
  (match markup
    [(cons s (cons (cons 'attr attrs) content))
     #:when (symbol? s)
     (make-node s attrs content)]
    [(cons s content) #:when (symbol? s)
                    (make-node s '() content)]
    [s #:when (string? s) (string-append s " ")]
    [s #:when (symbol? s) (string-append (symbol->string s) " ")]
    [n #:when (number? n) (string-append (number->string n) " ")]))

(define (make-node t attrs content)
  (string-append (make-tag t attrs)
                 (apply string-append (map ->html content))
                 (make-tag t attrs #t)))

(define (make-tag s attrs [close? #f])
    (string-append "<" (if close? "/" "") (symbol->string s) (make-attrs attrs) ">"))

;; Attrs is list of (name value) pairs
(define (make-attrs attrs)
  (define (make-attr attr)
    (match attr
      [(list name val) (string-append " " (symbol->string name) "=" "\"" val "\"")]
      [_ (error "needs to be list")]))
  (apply string-append (map make-attr attrs)))

