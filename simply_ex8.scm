;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;;Real exercises
; simply scheme chapter 8

(define (vowel? ch)
    (member? ch '(a i u e o))
)

(define (end-vowel? wd)
    (vowel? (last wd))
)

(define (even-count? wd)
    (even? (count wd)))

(define (choose-beatles func)
  (keep func '(john paul george ringo))
)

;8.5

(define (amazify name)
   (word 'the-amazing- name))

(define (transform-beatles func)
    (every func '(john paul george ringo)))


;; 8.6

(define (code-name letter)
    (cond ((equal? letter 'a) 'alpha)
          ((equal? letter 'b) 'bravo)
          ((equal? letter 'c) 'charlie)
          ))

(define (words wd)
    (every code-name wd))

;8.7
;(define (always-one i) 1)

(define (letter-count st)
    (accumulate + (every count st)))

;8.8
(define (double-down wd)
    (cond ((number? wd) (* 2 wd))
          ((equal? wd 'good) 'great)
          (else wd)))

(define (exaggerate st)
    (every double-down st))

;8.9
 (every word '(hello world))
(keep word? '(hello world))
(accumulate word '(hello world))

;; 8.10

(define (true-for-all? func lst)
    (= (count lst) (count (keep func lst))))

;; 8.11
(define (base-grade grd)
    (cond ((equal? grd 'a) 4)
          ((equal? grd 'b) 3)
          ((equal? grd 'c) 2)
          ((equal? grd 'd) 1)
          ((equal? grd 'e) 0)
          (else 0)))

(define (grade-modifier grd)
    (cond ((equal? '+ (last grd)) (+ (base-grade (first grd)) 0.33))
          ((equal? (last grd) '-) (- (base-grade (first grd)) 0.33))
          (else (base-grade grd))))

(define (gpa grd-list)
    (/ (accumulate + (every grade-modifier grd-list)) (count grd-list)  ))

;; 8.12
(define (um? wd)
    (equal? wd 'um))

(define (count-ums stc)
    (count (keep um? stc)))


;; 8.13
(define (unspell ltr)
    (cond ((or (equal? ltr 'a) (equal? ltr 'b) (equal? ltr 'c)) 2)
          ((or (equal? ltr 'd) (equal? ltr 'e) (equal? ltr 'f)) 3)
          ((or (equal? ltr 'g) (equal? ltr 'h) (equal? ltr 'i)) 4)
          ((or (equal? ltr 'j) (equal? ltr 'k) (equal? ltr 'l)) 5)
          ((or (equal? ltr 'm) (equal? ltr 'n) (equal? ltr 'o)) 6)
          ((or (equal? ltr 'p) (equal? ltr 'q) (equal? ltr 'r) (equal? ltr 's))  7)
          ((or (equal? ltr 't) (equal? ltr 'u) (equal? ltr 'v)) 8)
          ((or (equal? ltr 'w) (equal? ltr 'x) (equal? ltr 'y) (equal? ltr 'z)) 9)
     ))


(define (phone-unspell strphone)
    (accumulate word (every unspell strphone)))

;; 8.14
(define (subword str begin end)
    ((repeated bl (- (count str) end ))
      ((repeated bf (- begin 1)) str)))
 
