#|
10/03/2021

Ricardo Alonso Aróstegui A01029011
Agustín Pumarejo Ontañón A01028997
Adriana Abella Kuri A01329591
|#

#lang racket

(require racket/trace)

(define (validate-line line dfa init-state)
  (let loop
    ([lst (regexp-split #px"(?!\\#?\\w)" line)];([lst (regexp-split #px"\\b" line)]
    [state init-state] ; The init-state parameter
    [token-list empty]
    [transition (car dfa)] ; The first element in the list
    ) 
    (if (empty? lst)
      ; Check if the final state is in the list of acceptables
      (if (member state (caddr dfa))
        ; Return the list of tokens and the last accept state
        (values token-list state)
        #f)
      (let-values
        ([(state token-type) (transition state (car lst))])
        ; Recursive call
        (loop
          (cdr lst)
          state
          ; Add valid tokens to the list
          (if token-type
            (append token-list (list (list token-type (car lst) state)))
            token-list)
          ; Pass the same function again
          transition)))))



(define (validate-document init-list dfa)
  (let loop
    ([lst init-list]
    [result empty]
    [state (cadr dfa)])
    (if (empty? lst)
      result
      (let-values
        ([(token-list state) (validate-line (car lst) dfa state)])
        (loop
          (cdr lst)
          (append result (list token-list))
          state)))))



(define (css-tokens-validation state string)
  (cond
    [(eq? state 'selector) (cond
      [(regexp-match? #rx"^\\-?[a-zA-Z_]\\w*" string) (values 'selector 'element)]
      [(regexp-match? #rx"^\\.\\-?[a-zA-Z_]\\w*" string) (values 'selector 'class)]
      [(regexp-match? #rx"^\\#\\-?[a-zA-Z_]\\w*" string) (values 'selector 'id)]
      [(regexp-match? #rx"\\s*\\{" string) (values 'property 'b)]
      [else (values 'selector 'b)])]
    [(eq? state 'property) (cond
      [(regexp-match? #rx"\\s*\\}" string) (values 'selector 'b)]
      [(regexp-match? #rx"[a-z]+(\\-[a-z]+)*\\s*" string) (values 'property 'property)]
      [(regexp-match? #rx"\\s*\\:" string) (values 'value 'b)]
      [else (values 'property 'b)])]
    [(eq? state 'value) (cond
      [(regexp-match? #rx"\\s*\\}" string) (values 'selector 'b)]
      [(regexp-match? #rx"[a-z0-9]+(\\s+[a-z0-9]+)*\\s*" string) (values 'value 'value)]
      [(regexp-match? #rx"\\s*\\;" string) (values 'property 'b)]
      [else (values 'value 'b)])]
    [else (values 'b 'b)]))



(define (element-to-html element)
  (cond
    [(eq? (car element) 'element) (string-append "<span class='element'>" (cadr element))]
    [(eq? (car element) 'class) (string-append "<span class='class'>" (cadr element))]
    [(eq? (car element) 'id) (string-append "<span class='id'>" (cadr element))]
    [(eq? (car element) 'property) (string-append "<span class='property'>" (cadr element))]
    [(eq? (car element) 'value) (string-append "<span class='value'>" (cadr element))]
    [(eq? (car element) 'b) (string-append "<span>" (cadr element))]
    [else element]))



(define (line-tokens-to-html init-line)
  (let loop
    ([lst init-line]
    [result "<p>"])
    (if (empty? lst)
      (string-append result "</p>")
      (let-values
          ([(element) (car lst)])
          (loop
            (cdr lst)
            (string-append result (string-append (element-to-html element) "</span>")))))))



(define (document-tokens-to-html in-file-path out-file-path)
  (let loop
    ([lst (validate-document (file->lines in-file-path) (list css-tokens-validation 'selector (list 'selector 'property 'value 'b)))]
    [result '("<html><head><link type='text/css' rel='stylesheet' href='../Actividad 3.4/style.css'></head><body>")])
    (if (empty? lst)
      (append result (list "</body></html>"))
      (loop
        (cdr lst)
        (append result (list (line-tokens-to-html (car lst))))))))



; read from a file
(define (file-to-tokens in-file-path out-file-path)
  (display-lines-to-file
    (document-tokens-to-html in-file-path out-file-path)
    out-file-path
    #:exists 'truncate))



; (file-to-tokens "infile.txt" "outfile.html")