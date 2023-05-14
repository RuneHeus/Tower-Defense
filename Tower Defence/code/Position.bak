(define (make-position xCoordinate yCoordinate)

  (let ((x xCoordinate)
        (y yCoordinate))
    
    (define (set-x! num)
      (set! x num))

    (define (set-y! num)
      (set! y num))

    (define (equal? position)
      (and (= x (position 'get-x)) (= y (position 'get-y))))

    (define (in-area? entity-pos range)
      (and
       (and (>= (entity-pos 'get-x) (- x (* 50 range))) (>= (entity-pos 'get-y) (- y (* 50 range))))
       (and (<= (entity-pos 'get-x) (+ x (* 50 range))) (<= (entity-pos 'get-y) (+ y (* 50 range))))))

    (define (change-coordinates! num1 num2)
      (set! x num1)
      (set! y num2))

    (define (close-enough? target-pos)
      (let ((distance (sqrt (+ (expt (- (target-pos 'get-x) x) 2) (expt (- (target-pos 'get-y) y) 2)))))
        (< distance 20)))

    (define (outside-playarea? area-pos-x area-pos-y)
      (or (> x area-pos-x) (> y area-pos-y) (< x 0) (< y 0)))

    (define (display-position)
      (newline)
      (display "------Position-----")
      (newline)
      (display "x: ")
      (display x)
      (display ", y: ")
      (display y)
      (newline)
      (display "-------------------"))

    (define (dispatch mes)
      (cond ((eq? mes 'get-x) x)
            ((eq? mes 'get-y) y)
            ((eq? mes 'set-x!) set-x!)
            ((eq? mes 'set-y!) set-y!)
            ((eq? mes 'equal?) equal?)
            ((eq? mes 'change-coordinates!) change-coordinates!)
            ((eq? mes 'in-area?) in-area?)
            ((eq? mes 'close-enough?) close-enough?)
            ((eq? mes 'outside-playarea?) outside-playarea?)
            ((eq? mes 'display-position) display-position)
            (else (display "Error: Wrong dispatch message (Position.rkt) -> ") (display mes))))
    dispatch))