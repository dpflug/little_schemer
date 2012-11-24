(define (atom? a)
  (not (or
	(pair? a)
	(null? a))))

(define (lat? l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f)))

(define (member? m l)
  (cond
   ((null? l) #f)
   (or (equal? m (car l))
       (member? m (cdr l)))))

(define (rember a l)
  (cond
   ((null? l) '())
   ((equal? (car l) a) (cdr l))
   (else (cons (car l) (rember a (cdr l))))))

(define (firsts l)
  (cond
   ((null? l) '())
   (else (cons (car (car l)) (firsts (cdr l))))))

(define (insertR new old l)
  (cond
   ((null? l) '())
   ((equal? old (car l)) (cons old (cons new (cdr l))))
   (else (cons (car l) (insertR new old (cdr l))))))

(define (insertL new old l)
  (cond
   ((null? l) '())
   ((equal? old (car l)) (cons new l))
   (else (cons (car l) (insertL new old (cdr l))))))

(define (subst new old lat)
  (cond
   ((null? lat) '())
   ((equal? old (car lat)) (cons new (cdr lat)))
   (else (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or
     (equal? o1 (car lat))
     (equal? o2 (car lat)))
    (cons new (cdr lat)))
   (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat))))))))

(define (multiinsertR new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat))))))))

(define (multiinsertL new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat))))))))

(define (multisubst new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat))))))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (zero? n)
  (equal? 0 n))

(define (o+ n m)
  (cond
   ((sero? m) n)
   (else (edd1 (o+ n (zub1 m))))))

(define (o- n m)
  (cond 
   ((zero? m) n)
   (else (sub1 (o- n (sub1 m))))))

(define (addtup tup)
  (cond
   ((null? tup) 0)
   (else (o+ (car tup) (addtup (cdr tup))))))

(define (x n m)
  (cond
   ((zero? m) 0)
   (else (o+ n (x n (sub1 m))))))

(define (tup+ tup1 tup2)
  (cond
   ((null? tup1) tup2)
   ((null? tup2) tup1)
   (else
    (cons
     (o+ (car tup1) (car tup2))
     (tup+ (cdr tup1) (cdr tup2))))))

(define (> n m)
  (cond
   ((zero? n) #f)
   ((zero? m) #t)
   (else (> (sub1 n) (sub1 m)))))

(define (< n m)
  (cond
   ((zero? m) #f)
   ((zero? n) #t)
   (else (< (sub1 n) (sub1 m)))))

(define (= n m)
  (cond
   ((> n m) #f)
   ((< n m) #f)
   (else #t)))

(define (myexpt n m)
  (cond
   ((zero? m) 1)
   (else (x n (myexpt n (sub1 m))))))

(define (quot n m)
  (cond
   ((< n m) 0)
   (else (add1 (quot (o- n m) m)))))

(define (length lat)
  (cond
   ((null? lat) 0)
   (else (add1 (length (cdr lat))))))

(define (pick n lat)
  (cond
   ((zero? (sub1 n)) (car lat))
   (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond
   ((one? n) (cdr lat))
   (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond
   ((null? lat) '())
   ((number? (car lat)) (no-nums (cdr lat)))
   (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond
   ((null? lat) '())
   ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
   (else (all-nums (cdr lat)))))

(define (eqan? a1 a2)
  (cond
   ((and (number? a1) (number? a2))
    (= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (equal? a1 a2))))

(define (occur a lat)
  (cond
   ((null? lat) 0)
   ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
   (else (occur a (cdr lat)))))

(define (one? n)
  (equal? n 1))

(define (rember* a l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) a)
      (rember* a (cdr l)))
     (else (cons (car l)
		 (rember* a (cdr l))))))
   (else (cons (rember* a (car l))
	       (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? old (car l))
      (cons old (cons new (insertR* new old (cdr l))))))
    (else
     (cons (car l) (insertR* new old (cdr l)))))
   (else
    (cons (insertR* new old (car l))
	  (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond
   ((null? l) 0)
   ((atom? (car l))
    (cond
     ((equal? (car l) a)
      (add1 (occur* a (cdr l))))
     (else (occur* a (cdr l)))))
   (else
    (o+ (occur* a (car l)) (occur* a (cdr l))))))

(define (subst* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons new (subst* new old (cdr l))))
     (else (cons (car l) (subst* new old (cdr l))))))
   (else (cons (subst* new old (car l))
	       (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons new (cons old (insertL* new old (cdr l)))))
     (else (cons (car l) (insertL* new old (cdr l))))))
   (else (cons (insertL* new old (car l))
	       (insertL* new old (cdr l))))))

(define (member* a l)
  (cond
   ((null? l) #f)
   ((atom? (car l))
    (or
     (equal? (car l) a)
     (member* a (cdr l))))
   (else (or (member* a (car l)) (member* a (cdr l))))))

(define (leftmost l)
  (cond
   ((atom? (car l)) (car l))
   (else (leftmost (car l)))))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((or (null? l1) (null? l2)) #f)
   (else
    (and (equal? (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2))))))

(define (equal? s1 s2)
  (cond
   ((and (atom? s1) (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1) (atom? s2)) #f)
   (else (eqlist? s1 s2))))

(define (numbered? aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (else
    (and (numbered? (car aexp))
	 (numbered? (car (cdr (cdr aexp))))))))

(define (value nexp)
  (cond
   ((atom? nexp) nexp)
   ((equal? (operator nexp) '+) (+
			       (value (1st-sub-exp nexp))
			       (value (2nd-sub-exp nexp))))
   ((equal? (operator nexp) 'x) (*
			       (value (1st-sub-exp nexp))
			       (value (2nd-sub-exp nexp))))
   (else (myexpt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(define (1st-sub-exp aexp) (car aexp))

(define (2nd-sub-exp aexp) (car (cdr (cdr aexp))))

(define (operator aexp) (car (cdr aexp)))

(define (sero? n) (null? n))

(define (edd1 n) (cons '() n))

(define (zub1 n) (cdr n))

(define (set? lat)
  (cond
   ((null? lat) #t)
   ((member? (car lat) (cdr lat)) #f)
   (else (set? (cdr lat)))))

(define (makeset lat)
  (cond
   ((null? lat) '())
   (else (cons (car lat)
	       (makeset
		(multirember (car lat)
			     (cdr lat)))))))
