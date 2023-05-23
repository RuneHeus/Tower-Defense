;This file is all about procedures that dont belong anywhere
;There procedures are made to clear up some Racket files

(#%require (only racket/base random))

(define (object-eq-pos-obstacles? object obstacles)
  (let ((exist? #f))
    (if (null? obstacles)
        exist?
        (map (lambda (obstacle)
               (if (((object 'get-position) 'close-enough?) (obstacle 'get-position))
                   (begin (display "Close") (set! exist? obstacle)))) obstacles))
    exist?))

(define (print-el-in-list list)
  (if (not (null? list))
      (begin
        (display (car list))
        (display ", ")
        (print-el-in-list (cdr list)))))

(define (remove-el-from-list el list) ;Returns the list without the item
  (if (null? list)
      '()
      (if (eq? el (car list))
          (remove-el-from-list el (cdr list))
          (cons (car list) (remove-el-from-list el (cdr list))))))

(define (println string)
  (display string)
  (newline))

(define (add-element-to-list element lst)
  (if (null? lst)
      (list element)
      (append lst (list element))))

;(define (entity-passed-obstacle? entity obstacle)
;  (let ((passed? #f))
;    (map (lambda (item)
;           (if (eq? item obstacle)
;               (set! passed? #t)))
;         (entity 'get-passed-obstacles))
;    passed?))

(define (pick-random-from-list lst . but);but = Is the element(s) in the list that it can not chose
  (define (loop lst2 but2)
    (if (not (null? but2))
        (loop (remove-el-from-list (car but2) lst2) (cdr but2))
        (list-ref lst2 (random 0 (length lst2)))))
  (loop lst but))


(define (random-pos-between-points pos1 pos2)
  (let ((angle (atan (- (pos2 'get-y) (pos1 'get-y)) (- (pos2 'get-x) (pos1 'get-x))))
        (calculated-position '()))
    (if (= (round (cos angle)) 0)
        (let ((calculated-y (random (inexact->exact (+ (pos1 'get-y) 1)) (inexact->exact (pos2 'get-y)))))
          (if (< calculated-y 250)
              (set! calculated-y 250))
          (set! calculated-position (make-position (pos1 'get-x) calculated-y)))
        (set! calculated-position (make-position (random (inexact->exact (+ (pos1 'get-x) 1)) (inexact->exact (pos2 'get-x))) (pos1 'get-y))))
    calculated-position))

(define (create-target-pos target)
  (if (not (null? target))
      (make-position ((target 'get-position) 'get-x) ((target 'get-position) 'get-y))))

(define (set-tile-position! tile position)
  ((tile 'set-x!) (position 'get-x))
  ((tile 'set-y!) (position 'get-y)))