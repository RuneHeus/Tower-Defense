(load "Constants.rkt")

(define (make-wave environment)

  (define wave 0)
  
  (define wave-list '())
  
  (define (next-monster!)
    (if (not (null? wave-list))
        (begin ((environment 'add-entity!) (car wave-list))
               (set! wave-list (cdr wave-list))
               (set! wave-list (cons (make-monster (make-position (start-position 'get-x) (start-position 'get-y))) wave-list)))))

  (define (fill-wave-list n)
    (define (iter counter)
      (if (not (= counter n))
          (let ((monster (make-monster (make-position (start-position 'get-x) (start-position 'get-y)))))
            (set! wave-list (append wave-list (list monster)))
            (iter (+ counter 1)))))
    (iter 0))

  (define (set-wave! val)
    (set! wave val))

  (define (set-wave-list! val)
    (set! wave-list val))
  
  (define (load-wave!)
    (set! wave (+ wave 1))
    (cond ((= wave 1) (fill-wave-list 5))))
  
  (define (dispatch mes)
    (cond ((eq? mes 'next-monster!) next-monster!)
          ((eq? mes 'load-wave!) load-wave!)
          ((eq? mes 'get-wave) wave)
          ((eq? mes 'set-wave!) set-wave!)
          ((eq? mes 'get-wave-list) wave-list)
          ((eq? mes 'set-wave-list!) set-wave-list!)
          (else (display "Error: Wrong dispatch message (Wave.rkt) -> ") (display mes))))
  dispatch)