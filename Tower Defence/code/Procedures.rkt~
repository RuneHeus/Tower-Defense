;This file is all about procedures that dont belong anywhere
;There procedures are made to clear up some Racket files

(define (object-eq-pos-obstacles? object obstacles)
  (let ((exist? #f))
    (if (null? obstacles)
        exist?
        (map (lambda (obstacle)
               (if (((object 'get-position) 'close-enough?) (obstacle 'get-position))
                   (set! exist? obstacle))) obstacles))
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