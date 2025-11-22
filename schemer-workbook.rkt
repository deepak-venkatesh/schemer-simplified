;;;; Code belongs to The Little Schemer ;;;;

#lang racket
(require racket/trace)

;; Preface

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Book Ch02: Do It, Do It Again, and Again, and Again...

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                 (member? a (cdr lat)))))))

(define my-member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

;; Book Ch03: Cons the Magnificent

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define rember-wrong
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (rember-wrong a (cdr lat))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond ((null? l) (quote ()))
          (else (cons (car (cdr (car l)))
                      (seconds (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons new (cons (car lat) (cdr lat))))
          (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) (quote ()))
          ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
          (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))

;; Book Ch04: Number Games

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (o+ n (o* n (sub1 m)))))))

(define tup+v1       ; this v1 version will work only if length of tup1 and tup2 is same
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) (quote ()))
          (else (cons (o+ (car tup1) (car tup2)) (tup+v1 (cdr tup1) (cdr tup2)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond ((zero? m) (zero? n))
          ((zero? n) #f)
          (else (= (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond ((> n m) #f)
          ((< n m) #f)
          (else #t))))

(define o^
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (o* n (o^ n (sub1 m)))))))

(define o-div
  (lambda (n m)
    (cond ((< n m) 0)
          (else (add1 (o-div (o- n m) m))))))

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add1 (length (cdr lat)))))))

(define pick-zero
  (lambda (n lat)
    (cond ((zero? n) (car lat))
          (else (pick-zero (sub1 n) (cdr lat))))))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))
                           
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define no-nums-else
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (no-nums-else (cdr lat)))
              (else (cons (car lat)
                          (no-nums-else (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (cons
                            (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one-zero?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(define one-equal?
  (lambda (n)
    (cond
      (else (= n 1)))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick-one
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick-one (sub1 n) (cdr lat)))))))

;; Book Ch05: *Oh My Gawd*: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? a (car l)) (rember* a (cdr l)))
                 (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l))
                      (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? old (car l))
                  (cons old (cons new (insertR* new old (cdr l)))))
                 (else (cons (car l) (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
  (cond ((null? l) 0)
        ((atom? (car l))
         (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
               (else (occur* a (cdr l)))))
        (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons new (subst* new old (cdr l))))
                 (else (cons (car l)
                             (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons new (cons old (insertL* new old (cdr l)))))
                 (else (cons (car l) (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
           (or (eq? (car l) a)
               (member* a (cdr l))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))
                  
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          ((and (atom? (car l1)) (atom? (car l2)))
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          (else (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))))

(define my-equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2)) #f)
          (else (eqlist? s1 s2)))))

(define eqlist-equal?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (my-equal? (car l1) (car l2)))
                (eqlist-equal? (cdr l1) (cdr l2))))))

(define rember-equal
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember-equal s (cdr l)))))))


;; Book Ch06: Shadows


(define numbered-long?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          ((eq? (car (cdr aexp)) (quote o+))
           (and (numbered-long? (car aexp)) (numbered-long? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote o*))
           (and (numbered-long? (car aexp)) (numbered-long? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote o^))
           (and (numbered-long? (car aexp)) (numbered-long? (car (cdr (cdr aexp)))))))))

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (car (cdr nexp)) (quote +))
           (o+ (value (car nexp))
               (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote x))
           (o* (value (car nexp))
               (value (car (cdr (cdr nexp))))))
          (else
           (o^ (value (car nexp))
               (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-help
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) (quote +))
           (o+ (value-help (1st-sub-exp nexp))
               (value-help (2nd-sub-exp nexp))))
          ((eq? (operator nexp) (quote x))
           (o* (value-help (1st-sub-exp nexp))
               (value-help (2nd-sub-exp nexp))))
          (else
           (o^ (value (1st-sub-exp nexp))
               (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define o+rep
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (o+rep n (zub1 m)))))))


;; Book Ch07: Friends and Relations

(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((member? (car lat) (cdr lat))
           (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset-multirember
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          (else
           (cons (car lat)
                 (makeset-multirember
                  (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          ((member? (car set1) set2)
           (subset? (cdr set1) set2))
          (else #f))))

(define subset-and?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1) set2)
                     (subset-and? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          ((member? (car set1) set2) #t)
          (else (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2)
           (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

(define set-difference
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2)
           (set-difference (cdr set1) set2))
          (else (cons (car set1)
                      (set-difference (cdr set1) set2))))))

(define intersectall ;;smart function!
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set)
                           (intersectall (cdr l-set)))))))

(define a-pair?  ;;reason out each line of cond
  (lambda (x)
    (cond
      ((atom? x) #f) ;; there is not a single element
      ((null? x) #f) ;; the list is not empty
      ((null? (cdr x)) #f) ;; the list does not have only one element
      ((null? (cdr (cdr x))) #t) ;; there is no third or more element
                                 ;;i.e. now we have exactly 2 elements
      (else #f)))) ;; everything else is not a pair

(define my-first
  (lambda (p)
    (cond
      (else (car p)))))

(define my-second
  (lambda (p)
    (cond
      (else (car (cdr p))))))

(define my-third  ;; not applicable to pairs but in general for a list
  (lambda (p)
    (cond
      (else (car (cdr (cdr p)))))))

(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1 (cons s2 (quote ())))))))

(define my-third-nocond
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) (quote ()))
          (else (cons (build
                       (second (car rel))
                       (first (car rel)))
                      (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel-revpair
  (lambda (rel)
    (cond ((null? rel) (quote ()))
          (else (cons (revpair (car rel))
                      (revrel-revpair (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))


;; Book Ch08: Lambda the Ultimate

(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-f1 test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define insertL-anon
  (insert-g
   (lambda (new old l)
    (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-def2
  (insert-g seqS))

(define subst-anon
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define rember-helper
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define seqrem
  (lambda (new old l)
    l))

(define value-helper
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (o+ (value-helper (1st-sub-exp nexp))
           (value-helper (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x))
       (o* (value-helper (1st-sub-exp nexp))
           (value-helper (2nd-sub-exp nexp))))
      (else
       (o^ (value-helper (1st-sub-exp nexp))
           (value-helper (2nd-sub-exp nexp)))))))
    
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) o+)
      ((eq? x (quote x)) o*)
      (else o^))))

(define value-helperfn
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else
           ((atom-to-function (operator nexp))
            (value-helperfn (1st-sub-exp nexp))
            (value-helperfn (2nd-sub-exp nexp)))))))
            
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) (quote ()))
            ((test? a (car lat))
             ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) (quote ()))
          ((test? (car lat))
           (multiremberT test? (cdr lat)))
          (else (cons (car lat)
                      (multiremberT test? (cdr lat)))))))
              
(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col (quote ()) (quote ())))
          ((eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen)))))
          (else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat)
                                  seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))
           

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons (quote tuna) seen))))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      (eq? (car lat) oldL
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat)))))
      (eq? (car lat) oldR
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL newlat))
                                      (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat) L R)))))))

(define col
  (lambda (newlat L R)
    (cons newlat (cons L (cons R (quote ()))))))

(define even?
  (lambda (n)
    (cond
      ((= (modulo n 2) 0) #t)
      (else #f))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))
                             
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                      (* (car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (lambda (newl p s)
                                 (col newl p
                                           (+ (car l) s)))))))
      (else (evens-only*&co
             (car l) (lambda (al ap as)
                       (evens-only*&co
                        (cdr l) (lambda (dl dp ds)
                                  (col (cons al dl)
                                       (* ap dp)
                                       (+ as ds))))))))))
                                       
         
(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))


;; Book Ch09: ... and Again, and Again, and Again, ...

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
           (keep-looking a (pick sorn lat) lat))
          (else (eq? sorn a)))))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else
           (+ (length* (first pora))
              (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else
           (+ (* (weight* (first pora)) 2)
              (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else (add1
                  (eternity (cdr l))))))
      (cdr l))))))
                     
(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else (add1
                  ((lambda (l)
                     (cond
                       ((null? l) 0)
                       (else
                        (add1
                         (eternity
                          (cdr l))))))
                   (cdr l))))))
       (cdr l))))))

((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))
 
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
                      
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))
   
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length eternity) (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length mk-length) (cdr l))))))))
   

#|
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))
|#


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((lambda (x)
                 ((mk-length mk-length) x))
               (cdr l))))))))


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
              
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))



(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


;; Book Ch10: What is the value of all this?

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name)
       (car values))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))
#|
(cons rep-a
      (cons rep-b
            (cons rep-c (quote ()))))
|#


#|
(cons rep-car
      (cons (cons rep-quote
                  (cons
                   (cons rep-a
                         (cons rep-b
                               (cons rep-c
                                     (quote ()))))
                   (quote ())))
      (quote ())))
|#


((lambda (nothing)
   (cons nothing (quote ())))
 (quote
  (from nothing comes something)))

((lambda (nothing)
   (cond
     (nothing (quote something))
     (else (quote nothing))))
   #t)

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? #t) *const)
      ((eq? #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))
         
(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-value)))

(define initial-table
  (lambda (name)
    (car (qote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lins)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x  (quote else)))
          (else #f))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else (cons (meaning (car args) table)
                  (evlis (cdr args) table))))))
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       ((apply-primitive (second fun) vals))
       ((non-primitive? fun)
        (apply-closure (second fun) vals))))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons)) (cons (first vals) (second vals)))
      ((eq? name (quote car)) (car (first vals)))
      ((eq? name (quote cdr)) (cdr (first vals)))
      ((eq? name (quote null?)) (null? (first vals)))
      ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
      ((eq? name (quote atom?)) (:atom? (first vals)))
      ((eq? name (quote zero?)) (zero? (first vals)))
      ((eq? name (quote add1)) (add1 (first vals)))
      ((eq? name (quote sub1)) (sub1 (first vals)))
      ((eq? name (quote number?)) (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of clsoure)))))

;;;; End of Code in The Little Schemer ;;;;


