#|
Implement a Deterministic Finite Automata

Gilberto Echeverria
10/03/2021

Ricardo Alonso Aróstegui A01029011
Agustín Pumarejo Ontañón A01028997
Adriana Abella Kuri A01329591

(validate-string "4.2+1" (list accept-simple-arithmetic 'q0 (list 'int 'float 'space 'var)))
(validate-string "4.2 +1  - a_2" (list accept-simple-arithmetic 'q0 (list 'int 'float 'space 'var)))
|#

#lang racket

(require racket/trace)

(define (validate-string input-string dfa)
  " Determine if the input string is accepted by the dfa
  Ex: (validate-string 'abababa' (list accept-start-ba 'q0 '(q2)))
  Arguments:
  input-string - string
  dfa - list with these elements
            * transition function
            * start state
            * list of accept states
  Return: boolean "
  (let loop
    ([lst (string->list input-string)]
     [state (cadr dfa)]     ; The second element in the list
     [element ""]
     [token-list empty]
     [transition (car dfa)] ; The first element in the list
     ) 
    (if (empty? lst)
        ; Check if the final state is in the list of acceptables
        (if (member state (caddr dfa))
            ; Return the list of tokens and the last accept state
            (if (eq? state 'space)
                token-list
                (append token-list (list state element)))
            #f)
        (let-values
          ([(state token-type) (transition state (car lst))])
          ; Recursive call
          (loop
            (cdr lst)
            state
            (cond
              [(and (eq? token-type #f) (not(eq? (car lst) #\space))) (string-append element (string (car lst)))]
              [(not(eq? (car lst) #\space)) (string-append "" (string (car lst)))]
              [else ""])
            ; Add valid tokens to the list
            (if token-type
              (append token-list (list token-type) (list element))
              token-list)
            ; Pass the same function again
            transition)))))

(define (accept-simple-arithmetic state symbol)
  (let
    ([ops (list #\= #\+ #\- #\* #\/ #\^)])
    (cond
      [(eq? state 'q0) (cond
                         [(char-numeric? symbol) (values 'int #f)]
                         [(char-alphabetic? symbol) (values 'var #f)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'invalid #f)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'q0 #f)])]
      [(eq? state 'int) (cond
                         [(char-numeric? symbol) (values 'int #f)]
                         [(char-alphabetic? symbol) (values 'invalid #f)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'op 'int)]
                         [(eq? symbol #\.) (values 'dot #f)]
                         [(eq? symbol #\space) (values 'space 'int)])]
      [(eq? state 'dot) (cond
                         [(char-numeric? symbol) (values 'float #f)]
                         [(char-alphabetic? symbol) (values 'invalid #f)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'invalid #f)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'invalid #f)])]
      [(eq? state 'float) (cond
                         [(char-numeric? symbol) (values 'float #f)]
                         [(char-alphabetic? symbol) (values 'invalid #f)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'op 'float)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'space 'float)])]
      [(eq? state 'op) (cond
                         [(char-numeric? symbol) (values 'int 'op)]
                         [(char-alphabetic? symbol) (values 'var 'op)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'invalid #f)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'spaceOp 'op)])]
      [(eq? state 'space) (cond
                         [(char-numeric? symbol) (values 'invalid #f)]
                         [(char-alphabetic? symbol) (values 'invalid #f)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'op #f)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'space #f)])]
      [(eq? state 'spaceOp) (cond
                         [(char-numeric? symbol) (values 'int #f)]
                         [(char-alphabetic? symbol) (values 'var #f)]
                         [(eq? symbol #\_) (values 'invalid #f)]
                         [(member symbol ops) (values 'invalid #f)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'spaceOp #f)])]
      [(eq? state 'var) (cond
                         [(char-numeric? symbol) (values 'var #f)]
                         [(char-alphabetic? symbol) (values 'var #f)]
                         [(eq? symbol #\_) (values 'var #f)]
                         [(member symbol ops) (values 'op 'var)]
                         [(eq? symbol #\.) (values 'invalid #f)]
                         [(eq? symbol #\space) (values 'space 'var)])]
      [(eq? state 'invalid) (values 'invalid #f)])))