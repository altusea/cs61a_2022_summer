(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cddr s)))

(define (ascending? lst)
  (if (or (null? lst) (null? (cdr lst)))
      #t
      (and (<= (car lst) (car (cdr lst)))
           (ascending? (cdr lst)))))

(define (interleave lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      (append lst1 lst2)
      (cons (car lst1)
            (cons (car lst2)
                  (interleave (cdr lst1) (cdr lst2))))))

(define (my-filter func lst)
  (cond 
    ((null? lst)
     '())
    ((func (car lst))
     (cons (car lst) (my-filter func (cdr lst))))
    (else
     (my-filter func (cdr lst)))))

(define (no-repeats lst)
  (if (null? lst)
      lst
      (cons (car lst)
            (no-repeats
             (my-filter (lambda (x) (not (= (car lst) x)))
                        (cdr lst))))))
