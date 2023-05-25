(load "Position.rkt")
(#%require (only racket/base random))

(define (make-projectile type position target tower)
  
  (let ((tile (make-tile projectile-image-size projectile-image-size standard-projectile-img standard-projectile-mask))
        (target-pos '())
        (angle 0)
        (damage 1)
        (timer 3000)
        (move? #t)
        (speed 8)
        (behaviour '())
        (obstacle? #f)
        (exploded? #f)
        (projectile? #t)
        (follow? #t))

    (case type
      ('net (begin
              (set! tile (make-tile net-image-size net-image-size net-projectile-img net-projectile-mask))
              (set! damage 0)
              (set! target-pos (create-target-pos target))
              (set! obstacle? #t)
              (set! behaviour (lambda (monster)
                                ((monster 'set-speed!) (/ (monster 'get-default-speed) 2))
                                ((monster 'set-infection!) infection-duration)))))
      ('bomb (begin
               (set! tile (make-tile net-image-size net-image-size bomb-img bomb-mask))
               (if (not (eq? target 'Dummy-target))
                   (begin
                     (set! target-pos (create-target-pos target))
                     (set! obstacle? #t)
                     (set! damage 0)
                     (set! timer 2000)
                     (set! behaviour (lambda (monsters dmg ms draw)
                                       (((draw 'get-power-up-layer) 'remove-drawable!) tile)
                                       (set-tile! (make-tile explosion-size explosion-size explosion-img explosion-mask))
                                       ((draw 'reposition!) tile position 2)
                                       (((draw 'get-power-up-layer) 'add-drawable!) tile)
                                       (map (lambda (monster)
                                              (if ((position 'in-area?) (monster 'get-position) explosion-range)
                                                  ((monster 'hit!) dmg)))
                                            monsters)
                                       (set! exploded? #t)
                                       (minus-timer ms)))))))
      ('shooter (begin
                  (set! tile (make-tile projectile-image-size projectile-image-size shooter-projectile-img shooter-projectile-mask))
                  (set! follow? #f)
                  ((position 'set-distance-num!) 50)
                  (set! timer 1000))))

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
            (if follow?
                (calculate-move!))
            (if (and (not (eq? type 'shooter)) (not (null? (target 'get-position))))
                (if ((position 'close-enough?) (target 'get-position) #t)
                    (begin
                      ;((position 'display-compare-position) (target 'get-position))
                      (if obstacle? ;Is the projectile a obstacle?
                          (set-move! #f))
                      ((target 'hit!) damage)
                      (if (or (eq? type 'bomb) (eq? type 'net))
                          (((tower 'get-environment) 'add-obstacle) (tower 'get-projectile)))
                      (remove-projectile)))
                (map (lambda (monster)
                       (if ((position 'close-enough?) (monster 'get-position))
                           (begin
                             ((monster 'hit!) damage)
                             (remove-projectile))))
                     (environment 'get-monsters)))
            (if ((position 'outside-playarea?) width height)
                (remove-projectile)
                (begin
                  ((position 'change-coordinates!) (+ (position 'get-x) (* speed (cos angle))) (+ (position 'get-y) (* speed (sin angle))))
                  (((environment 'draw) 'reposition!) tile position)
                  (if (not (null? target))
                      (if ((position 'overshooting?) (target 'get-position) angle speed)
                          ((position 'change-coordinates!) ((target 'get-position) 'get-x) ((target 'get-position) 'get-y)))))))))
    
    (define (remove-projectile) ;If projectile = obstacle, delete it from the environment list
      ((((environment 'draw) 'projectile-layer) 'remove-drawable!) tile)
      (if (not (eq? type 'shooter))
          ((tower 'set-projectile!) #f)
          ((tower 'remove-projectile-from-list) dispatch)))

    (define (minus-timer value)
      (set! timer (- timer value)))

    (define (set-move! val)
      (set! move? val))

    (define (set-tile! new)
      (set! tile new))

    (define (set-angle! val)
      (set! angle val))

    (define (set-environment! env)
      (set! environment env))

    (define (set-projectile? val)
      (set! projectile? val))
    
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
            ((eq? mes 'set-tile!) set-tile!)
            ((eq? mes 'entity?) 'power-up)
            ((eq? mes 'set-angle!) set-angle!)
            ((eq? mes 'exploded?) exploded?)
            ((eq? mes 'set-projectile?) set-projectile?)
            ((eq? mes 'projectile?) projectile?)
            (else (display "Error: Wrong dispatch message (Projectile.rkt) -> ") (display mes))))
    (set-scale)
    (if (and (not (null? target)) (not (eq? target 'Dummy-target)))
        (begin
          (if (not follow?)
              (calculate-move!))
          (move!)))
    dispatch))