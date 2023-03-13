(define (make-position xCoordinate yCoordinate)

  (let ((x xCoordinate)
        (y yCoordinate))
    
    (define (set-x! num)
      (set! x num))

    (define (set-y! num)
      (set! y num))

    (define (equal? position)
      (and (= x (position 'get-x)) (= y (position 'get-y))))

    (define (in-area? position)
      (and (>= x (position 'get-x)) (>= y (position 'get-y))))

    (define (change-coordinates! num1 num2)
      (begin (set! x num1) (set! y num2)))

    (define (dispatch mes)
      (cond ((eq? mes 'get-x) x)
            ((eq? mes 'get-y) y)
            ((eq? mes 'set-x!) set-x!)
            ((eq? mes 'set-y!) set-y!)
            ((eq? mes 'equal?) equal?)
            ((eq? mes 'change-coordinates!) change-coordinates!)
            ((eq? mes 'in-area?) in-area?)
            (else (display "Error: Wrong dispatch message (Position.rkt) -> ") (display mes))))
    dispatch))