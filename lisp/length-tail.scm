;; If object is...
;; * non-null and non-pair  returns 0; object
;; * the empty list         returns 0; ()
;; * a proper list          returns length; ()
;; * a dotted list          returns length; cdr of last pair
;; * a circular list        returns length before cycle; first pair of cycle
;;
;; The precise meaning of "length" is "number of pairs". Since list
;; elements are stored in the "car" of each pair, the number of pairs
;; is equal to the number of elements.
;;
;; <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare>
;;
(define (length-tail object)
  (let detect-cycle ((len 0) (slow object) (fast object))
    (cond ((not (pair? fast))
           (values (* 2 len) fast))
          ((not (pair? (cdr fast)))
           (values (+ 1 (* 2 len)) (cdr fast)))
          ((not (and (eq? slow fast) (> len 0)))
           (detect-cycle (+ 1 len) (cdr slow) (cdr (cdr fast))))
          (else
           (let find-cycle-start ((len 0) (slow object) (fast fast))
             (if (eq? slow fast) (values len slow)
                 (find-cycle-start (+ len 1) (cdr slow) (cdr fast))))))))

(define (proper-list? object)
  (let-values (((_ tail) (length-tail object)))
    (null? tail)))

(define (circular-list? object)
  (let-values (((_ tail) (length-tail object)))
    (pair? tail)))

(define (dotted-list? object)
  (let-values (((len tail) (length-tail object)))
    (not (or (null? tail) (pair? tail)))))

;;

(define (circular list)
  (let loop ((last list))
    (cond ((null? (cdr last)) (set-cdr! last list) list)
          (else (loop (cdr last))))))

(define-syntax pv
  (syntax-rules (a b)
    ((_ expr) (let-values (((a b) expr))
                (write 'expr) (display " => ") (write a) (display "; ")
                (write b) (newline)))))

(pv (length-tail '()))
(pv (length-tail '(1)))
(pv (length-tail '(1 2)))
(pv (length-tail '(1 2 3)))
(pv (length-tail '(1 2 3 4)))

(pv (length-tail 2))
(pv (length-tail '(1 . 2)))
(pv (length-tail '(1 2 . 3)))
(pv (length-tail '(1 2 3 . 4)))

(pv (length-tail (circular '(1 2 3 4))))
(pv (length-tail (append '(1) (circular '(1)))))
(pv (length-tail (append '(1) (circular '(1 2)))))
(pv (length-tail (append '(1) (circular '(1 2 3)))))
(pv (length-tail (append '(1) (circular '(1 2 3 4)))))
(pv (length-tail (append '(1 2) (circular '(1 2 3 4)))))
(pv (length-tail (append '(1 2 3) (circular '(1 2 3 4)))))
