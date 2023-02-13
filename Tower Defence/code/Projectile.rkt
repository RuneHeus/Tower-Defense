(#%require "Graphics.rkt")
(load "Position.rkt")
(load "Constants.rkt")

(define (make-projectile position target environment)
  (let ((tile (make-tile 25 25 "../images/Projectiles/projectile.png" "../images/Projectiles/projectile_mask.png"))
        (move-x 0)
        (move-y 1))

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
        (set! move-x (/ calculated-x calculated-y))))

    (define (move!)
      (display "Previous pos: ")
      (display (position 'get-x))
      (display ",")
      (display (position 'get-y))
      (display "    ")
      (if ((position 'equal?) (target 'get-position))
          (begin
            (target 'hit!)
            (remove-projectile))
          ((position 'change-coordinates!) (+ (position 'get-x) move-x) (- (position 'get-y) move-y)))
      (display "Next pos: ")
      (display (position 'get-x))
      (display ",")
      (display (position 'get-y))
      (newline))
    
    (define (remove-projectile) ;via environment zoeken naar de toren en zo het projectiel verwijderen
      ((((environment 'draw) 'projectile-layer) 'remove-drawable!) tile)
      ;remove projectile from tower
      )
    
    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            (else (display "Error: Wrong dispatch message (Projectile.rkt) -> ") (display mes))))
    (set-scale)
    (calculate-move!)
    (move!)
    dispatch))