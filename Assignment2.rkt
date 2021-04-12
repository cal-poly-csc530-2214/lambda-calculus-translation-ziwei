#lang typed/racket
(require typed/rackunit)

;;Author: Ziwei Wu
;;CSC 530 Assignment 2

#|LC	 	=	 	num
                |	 	id
                |	 (	(/ id => LC)
                |	 	(LC LC)
                |	 	(+ LC LC)
                |	 	(* LC LC)
                |	 	(ifleq0 LC LC LC)
                |	 	(println LC)|#

(define-type LC (U numC idC lambdaC funcCallC additionC multiplicationC ifleq0C println))
(struct numC ([num : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct lambdaC ([input : idC][func : LC])#:transparent)
(struct funcCallC([function : LC][arg : LC])#:transparent)
(struct additionC ([num1 : LC][num2 : LC])#:transparent)
(struct multiplicationC ([num1 : LC][num2 : LC])#:transparent)
(struct ifleq0C ([a : LC][b : LC][c : LC])#:transparent)
(struct println ([printArg : LC])#:transparent)

(define (sToPython [s : Sexp]): String
  (LCToPython (parse s)))

;;parse the lambda function and create an AST
(define (parse [s : Sexp]) : LC
  (match s
    [(? real? n) (numC n)]
    [(? symbol? s) (idC s)]
    [(list '/ id '=> lc) (lambdaC (cast (parse id) idC) (parse lc))]
    [(list 'println lc) (println (parse lc))]
    [(list lc1 lc2) (funcCallC (parse lc1) (parse lc2))]
    [(list '+ num1 num2) (additionC (parse num1) (parse num2))]
    [(list '* num1 num2) (multiplicationC (parse num1) (parse num2))]
    [(list 'ifleq0 lc1 lc2 lc3)(ifleq0C (parse lc1) (parse lc2) (parse lc3))]
    [_ ((error 'parse "DXUQ invalid statement: ~e" s))]))

;;turn it into valid python
(define (LCToPython [exp : LC]) : String
  (match exp
    [(numC num)(number->string num)]
    [(idC sym) (symbol->string sym)]
    [(lambdaC in func)(string-append "(lambda " (LCToPython in) ": " (LCToPython func) ")")]
    [(funcCallC func arg)(string-append (LCToPython func) "(" (LCToPython arg) ")")]
    [(additionC num1 num2) (string-append "(" (LCToPython num1) " + " (LCToPython num2) ")")]
    [(multiplicationC num1 num2) (string-append "(" (LCToPython num1) " * " (LCToPython num2) ")")]
    [(ifleq0C a b c) (string-append (LCToPython b) " if " (LCToPython a) " <= 0 else " (LCToPython c))]
    [(println p)(string-append "print(" (LCToPython p) ")")]))

(check-equal? (sToPython '2) "2")
(check-equal? (sToPython '(ifleq0 -2 (+ 2 3) -20)) "(2 + 3) if -2 <= 0 else -20")
(check-equal? (sToPython '((/ x => (+ x 14)) 4)) "(lambda x: (x + 14))(4)")
(check-equal? (sToPython '(println 4)) "print(4)")
(check-equal? (sToPython '((/ x => (* x 5)) 4)) "(lambda x: (x * 5))(4)")
(check-equal? (sToPython '((((/ x => (/ y => (/ z => (* x (+ y z))))) 4 ) 3) 2)) "(lambda x: (lambda y: (lambda z: (x * (y + z)))))(4)(3)(2)")