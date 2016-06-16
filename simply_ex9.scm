
;;9.2
(define second 
    (lambda (stuff) (first (bf stuff))))

(define make-adder (lambda (x) (+ num x)))

;;9.3
;; get the last element

;;9.4
(define (who sent)
    (every (describe sent) '(pete roger john keith)))

(define (describe sent)
    (lambda (person) (se person sent)))

;;9.5
(define (prepend-every char sent)
    (every (lambda (wd) (word char wd)) sent  ))
 
;;9.6

(define (sentence-version fn)
    (lambda (sent) (every fn sent)))
