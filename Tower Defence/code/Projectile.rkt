(load "Position.rkt")

(define (make-projectile type position target tower)
  
  (let ((tile (make-tile projectile-image-size projectile-image-size standard-projectile-img standard-projectile-mask))
        (target-pos (make-position ((target 'get-position) 'get-x) ((target 'get-position) 'get-y)))
        (angle 0)
        (damage 1)
        (cooldown 3000)
        (move? #t)
        (behaviour '())
        (obstacle? #f))


    (case type
      ("net" (begin
               (set! tile (make-tile net-image-size net-image-size net-projectile-img net-projectile-mask))
               (set! damage 0)
               (set! obstacle? #t)
               (set! behaviour (lambda (monster)
                                 ((monster 'set-speed!) (/ (monster 'get-speed) 2))
                                 ((monster 'set-infection!) infection-duration))))))

    (define (set-scale) ;This sets the scale of projectile
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (calculate-move!)
      (let* ((target-x ((target 'get-position) 'get-x)) ;The new target position
             (target-y ((target 'get-position) 'get-y)) ;The new target position
             (x (position 'get-x))
             (y (position 'get-y))
             (calculated-x (- target-x x))
             (calculated-y (- target-y y)))
        (set! angle (atan calculated-y calculated-x))))

    (define (move!)
      (if move?
          (begin (calculate-move!)
                 (if (not (null? (target 'get-position)))
                     (if ((position 'close-enough?) (target 'get-position))
                         (if (eq? (target 'get-infection) #f)
                             (begin
                               ((target 'hit!) damage)
                               ;(behaviour target)
                              ; (set! move? #f)
                               ((target 'set-infection!) 5000)
                               (if (<= cooldown 0)
                                   (remove-projectile))))
                         (if ((position 'outside-playarea?) width height)
                             (remove-projectile)
                             (begin
                               ((position 'change-coordinates!) (+ (position 'get-x) (* 6 (cos angle))) (+ (position 'get-y) (* 6 (sin angle))))
                               (((environment 'draw) 'reposition!) tile position))))))))
    
    (define (remove-projectile)
      (if obstacle?
          ((environment 'set-obstacles!) (remove-el-from-list dispatch (environment 'get-obstacles))))
      ((((environment 'draw) 'projectile-layer) 'remove-drawable!) tile)
      ((tower 'set-projectile!) #f))

    (define (minus-cooldown value)
      (set! cooldown (- cooldown value)))

    (define (set-move! val)
      (set! move? val))
    
    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'get-position) position)
            ((eq? mes 'move!) move!)
            ((eq? mes 'remove-projectile) remove-projectile)
            ((eq? mes 'get-behaviour) behaviour)
            ((eq? mes 'minus-cooldown) minus-cooldown)
            ((eq? mes 'get-cooldown) cooldown)
            ((eq? mes 'obstacle?) obstacle?)
            ((eq? mes 'get-move) move?)
            ((eq? mes 'set-move!) set-move!)
            (eelse (display "Error: Wrong dispatch message (Projectile.rkt) -> ") (display mes))))
    (set-scale)
    (move!)
    dispatch))