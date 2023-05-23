(define (make-wave environment)
  (let ((wave 0)
        (wave-list '())
        (wave-ready? #t))
  
    (define (next-monster!)
      (if (not (null? wave-list))
          (begin ((environment 'add-entity!) (car wave-list))
                 (set! wave-list (cdr wave-list))
                 ;(set! wave-list (cons (make-monster "Blue" (make-position (start-position 'get-x) (start-position 'get-y))) wave-list))
                 )))

    (define (fill-wave-list monster-list) ;List with monster and amount
      (define (iter lijst)
        (if (not (null? lijst))
            (let ((monster-cons (car lijst)))
              (do ((i 0 (+ i 1)))
                ((= i (cdr monster-cons)))      ; maybe return the last value of the iteration
                (set! wave-list (add-element-to-list (make-monster (car monster-cons) (make-position (start-position 'get-x) (start-position 'get-y))) wave-list))))
            (iter (cdr lijst))))
      (iter monster-list))

    (define (start-wave!)
      (set-wave! 0)
      (set-wave-list! '())
      (load-wave!))

    (define (set-wave! val)
      (set! wave val))

    (define (set-wave-list! val)
      (set! wave-list val))
  
    (define (load-wave!)
      (set! wave (+ wave 1))
      (cond ((= wave 1) (fill-wave-list (list (cons "Red" 3))))
            ((= wave 2) (fill-wave-list (list (cons "Blue" 1))))
            ((= wave 3) (begin
                          (fill-wave-list (list (cons "Gray" 1)))
                          (fill-wave-list (list (cons "Red" 3)))
                          (fill-wave-list (list (cons "Blue" 2)))))
            ((= wave 4) (begin
                          (fill-wave-list (list (cons "Purple" 1)))
                          (fill-wave-list (list (cons "Blue" 3))))))
      (set! wave-ready? #f))

    (define (set-wave-ready! val)
      (set! wave-ready? val))
  
    (define (dispatch mes)
      (cond ((eq? mes 'next-monster!) next-monster!)
            ((eq? mes 'load-wave!) load-wave!)
            ((eq? mes 'get-wave) wave)
            ((eq? mes 'set-wave!) set-wave!)
            ((eq? mes 'get-wave-list) wave-list)
            ((eq? mes 'set-wave-list!) set-wave-list!)
            ((eq? mes 'start-wave!) start-wave!)
            ((eq? mes 'wave-ready?) wave-ready?)
            ((eq? mes 'set-wave-ready!) set-wave-ready!)
            (else (display "Error: Wrong dispatch message (Wave.rkt) -> ") (display mes))))
    dispatch))