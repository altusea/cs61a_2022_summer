(define (cadr lst) (car (cdr lst)))

(define (make-kwlist1 keys values)
  (cons keys (cons values nil)))

(define (get-keys-kwlist1 kwlist) (car kwlist))

(define (get-values-kwlist1 kwlist) (cadr kwlist))

(define (make-kwlist2 keys values)
  (if (or (null? keys) (null? values))
      nil
      (cons (cons (car keys) (cons (car values) nil))
            (make-kwlist2 (cdr keys) (cdr values)))))

(define (get-keys-kwlist2 kwlist)
  (map car kwlist))

(define (get-values-kwlist2 kwlist)
  (map cadr kwlist))

(define (add-to-kwlist kwlist key value)
  (make-kwlist
   (append (get-keys-kwlist kwlist) (cons key nil))
   (append (get-values-kwlist kwlist)
           (cons value nil))))

(define (get-first-from-kwlist kwlist key)
  (let ((keys (get-keys-kwlist kwlist))
        (values (get-values-kwlist kwlist)))
    (cond 
      ((or (null? keys) (null? values))
       nil)
      ((equal? key (car keys))
       (car values))
      (else
       (get-first-from-kwlist
        (make-kwlist (cdr keys) (cdr values))
        key)))))

(define (prune-expr expr)
  (define (prune-helper lst)
    (cond 
      ((null? lst)
       '())
      ((null? (cdr lst))
       (cons (car lst) '()))
      (else
       (cons (car lst) (prune-helper (cdr (cdr lst)))))))
  (cons (car expr) (prune-helper (cdr expr))))

(define (curry-cook formals body)
  (cond 
    ((null? formals))
    (else
     (lambda (cons (car formals) nil)
       (curry-cook (cdr formals) body)))))

(define (curry-consume curries args)
  'YOUR-CODE-HERE)
