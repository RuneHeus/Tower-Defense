
(load "Position.rkt")
(load "Constants.rkt")

(define (make-projectile position target environment)
  (let ((tile (make-tile 25 25 "../images/Projectiles/projectile.png" "../images/Projectiles/projectile_mask.png"))
        (target-pos (make-position ((target 'get-position) 'get-x) ((target 'get-position) 'get-y)))
        (angle 0))

    (define (set-scale) ;This sets the scale of projectile
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (calculate-move!)
      (let* ((target-x ((target 'get-position) 'get-x))
             (target-y ((target 'get-position) 'get-y))
             (x (position 'get-x))
             (y (position 'get-y))
             (calculated-x (- target-x x))
             (calculated-y (- target-y y)))
        (set! angle (atan calculated-y calculated-x))))

    (define (move!)
      (calculate-move!)
;      (display "Projectile pos: ")
;      (display (position 'get-x))
;      (display ", ")
;      (display (position 'get-y))
;      (newline)
      (if ((position 'close-enough?) (target 'get-position))
          (begin
            ((target 'hit!))
            (remove-projectile))
          (if ((position 'outside-playarea?) width height)
              (remove-projectile)
              (begin
                ((position 'change-coordinates!) (+ (position 'get-x) (* 6 (cos angle))) (+ (position 'get-y) (* 6 (sin angle))))
                (((environment 'draw) 'reposition!) tile position)))))
    
    (define (remove-projectile)
      ((((environment 'draw) 'projectile-layer) 'remove-drawable!) tile)
      ((((environment 'find-tower-by-projectile) position) 'set-projectile!) #f))
    
    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'get-position) position)
            ((eq? mes 'move!) move!)
            (else (display "Error: Wrong dispatch message (Projectile.rkt) -> ") (display mes))))
    (set-scale)
    (move!)
    dispatch))