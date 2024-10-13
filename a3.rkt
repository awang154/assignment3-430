#lang typed/racket
(require rackunit)

(require (submod ".." ts))
  (tstruct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]))
  (define-type ExprC (U NumC IdC AppC PlusC MultC))
  (tstruct NumC ([n : Real]))
  (tstruct IdC ([s : Symbol]))
  (tstruct AppC ([fun : Symbol] [arg : ExprC]))
  (tstruct PlusC ([l : ExprC] [r : ExprC]))
  (tstruct MultC ([l : ExprC] [r : ExprC]))


(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  <interp-body>)

(define (parse [prog : Sexp]) : ExprC
  (match prog
    ;; <num>
    [(? real? n) (NumC n)]
    ;; {+ <expr> <expr>}
    [(list '+ l r) (PlusC (parse l) (parse r))]
    ;; {* <expr> <expr>}
    [(list '* l r) (MultC (parse l) (parse r))]
    ;; Catch-all for invalid syntax
    [other (error 'parse "expected valid syntax, got ~e" other)]))

(define (parse-fundef [prog : Sexp]) : FundefC
  (match prog
    ;; <num>
    [(? real? n) (NumC n)]
    ;; {+ <expr> <expr>}
    [(list '+ l r) (PlusC (parse l) (parse r))]
    ;; {* <expr> <expr>}
    [(list '* l r) (MultC (parse l) (parse r))]
    ;; Catch-all for invalid syntax
    [other (error 'parse "expected valid syntax, got ~e" other)]))

(define (parse-prog [s : Sexp]) : (listof FundefC)
  (match prog
    ;; <num>
    [(? real? n) (NumC n)]
    ;; {+ <expr> <expr>}
    [(list '+ l r) (PlusC (parse l) (parse r))]
    ;; {* <expr> <expr>}
    [(list '* l r) (MultC (parse l) (parse r))]
    ;; Catch-all for invalid syntax
    [other (error 'parse "expected valid syntax, got ~e" other)]))

(define (interp-fns [funs : (listof Fundefc)]) : Real
  (match exp
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC l r) (* (interp l) (interp r))]))

(define (interp [exp : ExprC] [funs : (listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC l r) (* (interp l) (interp r))]))

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))
