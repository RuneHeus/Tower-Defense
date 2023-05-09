(load "Position.rkt")
(#%require (only racket/base random))

(define (make-projectile type position target tower)
  
  (let ((tile (make-tile projectile-image-size projectile-image-size standard-projectile-img standard-projectile-mask))
        (target-pos (create-target-pos target))
        (angle 0)
        (damage 1)
        (timer 3000)
        (move? #t)
        (behaviour '())
        (obstacle? #f))

    (case type
      ("net" (begin
               (set! tile (make-tile net-image-size net-image-size net-projectile-img net-projectile-mask))
               (set! damage 0)
               (set! obstacle? #t)
               (set! behaviour (lambda (monster)
                                 ((monster 'set-speed!) (/ (monster 'get-default-speed) 2))
                                 ((monster 'set-infection!) infection-duration)))))
      ("bomb" (begin
                (set! tile (make-tile net-image-size net-image-size bomb-img bomb-mask))
                (set! obstacle? #t)
                (set! damage (random 1 3))
                (set! behaviour (lambda (monster) ((monster 'hit!) damage)))))) ;Random 1 or 2

    (define (set-scale) ;This sets the scale of projectile
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (calculate-move!)
      (let* ((target-x ((target 'get-position) 'get-x)) ;The new target position
             (target-y ((target 'get-position) 'get-y)) ;The new target position
             (x (- (position 'get-x) 20))
             (y (- (position 'get-y) 20))
             (calculated-x (- target-x x))
             (calculated-y (- target-y y)))
        (set! angle (atan calculated-y calculated-x))))

    (define (move!)
      (if move?
          (begin
            (if (not (null? (target 'get-position)))
                (if ((position 'close-enough?) (target 'get-position))
                    (begin
                      (if obstacle? ;Is the projectile a obstacle?
                          (set-move! #f))
                      ((target 'hit!) damage))))
            (if ((position 'outside-playarea?) width height)
                (remove-projectile)
                (begin
                  ((position 'change-coordinates!) (+ (position 'get-x) (* 10 (cos angle))) (+ (position 'get-y) (* 10 (sin angle))))
                  (((environment 'draw) 'reposition!) tile position))))))
    
    (define (remove-projectile)
      (if obstacle?
          ((environment 'set-obstacles!) (remove-el-from-list dispatch (environment 'get-obstacles)))) ;If projectile = obstacle, delete it from the environment list
      ((((environment 'draw) 'projectile-layer) 'remove-drawable!) tile)
      ((tower 'set-projectile!) #f))

    (define (minus-timer value)
      (set! timer (- timer value)))

    (define (set-move! val)
      (set! move? val))
    
    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'get-position) position)
            ((eq? mes 'move!) move!)
            ((eq? mes 'remove-projectile) remove-projectile)
            ((eq? mes 'get-behaviour) behaviour)
            ((eq? mes 'minus-time!) minus-timer)
            ((eq? mes 'get-timer) timer)
            ((eq? mes 'obstacle?) obstacle?)
            ((eq? mes 'get-move) move?)
            ((eq? mes 'set-move!) set-move!)
            ((eq? mes 'get-type) type)
            (else (display "Error: Wrong dispatch message (Projectile.rkt) -> ") (display mes))))
    (set-scale)
    (if (not (null? target))
        (begin
          (calculate-move!)
          (move!)))
    dispatch))