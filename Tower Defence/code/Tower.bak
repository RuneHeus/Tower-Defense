(#%require "Graphics.rkt")
(load "Position.rkt")
(load "Projectile.rkt")

(define (make-tower type position environment)

  (let ((tile (make-tile image-size image-size standard-tower-img standard-tower-mask))
        (area '())
        (range 2)
        (cooldown 0)
        (cooldown-time 2000)
        (target #f)
        (projectile #f)
        (projectile-type '())
        (check-area? #t)
        (cost 100))

    (case type
      (4 (begin ;Net Tower
           (set! tile (make-tile image-size image-size net-tower-img net-tower-mask))
           (set! projectile-type 'net)
           (set! cooldown-time 3000)
           (set! cost 100)))

      (5 (begin ;8 Shooter tower
           (set! tile (make-tile image-size image-size bullet-tower-img bullet-tower-mask))
           (set! projectile-type 'shooter)
           (set! cooldown-time 1000)
           (set! cost 200)))

      (6 (begin ;Bomb Tower
           (set! tile (make-tile image-size image-size bom-tower-img bom-tower-mask))
           (set! projectile-type 'bomb)
           (set! cooldown-time 2000)
           (set! cost 200))))
   
    (define (set-scale!) ;This sets the scale of the tower
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (check-area monster)
      (if (or (not target) (eq? monster target));Checks if a monster is in its area or not, if there is a monster in the area but the tower already has a target, than this monster will be ignored
          (let* ((x ((monster 'get-position) 'get-x))
                 (y ((monster 'get-position) 'get-y))
                 (monster-pos (make-position (- x (modulo x (* size-factor 50))) (- y (modulo y (* size-factor 50)))))
                 (change? #f))
            (if ((position 'in-area?) monster-pos range)
                (set! change? #t))
            (cond ((and (not target) change?)
                   (set! target monster))
                  ((and (eq? target monster) (not change?))
                   (set! target #f))))))

    (define (remove-target monster)
      (if (eq? monster target)
          (set! target #f)))
    
    (define (shoot!)
      (if (or target (= type 5)) ;If the tower has a target
          (begin
            (if (= cooldown 0)
                (if projectile
                    (if (list? projectile)
                        (map (lambda (proj)
                               ((proj 'move!)))
                             projectile)
                        ((projectile 'move!)))
                    (if (= type 5)
                        (begin
                          (remove-all-projectiles)
                          (add-all-projectiles))
                        (begin
                          (let ((new-projectile (make-projectile projectile-type (make-position (position 'get-x) (position 'get-y)) target dispatch)))
                            (set-projectile! new-projectile)
                            ((((environment 'draw) 'projectile-layer) 'add-drawable!) (projectile 'get-tile))
                            (set! cooldown cooldown-time)))))
                (if projectile
                    (if (list? projectile)
                        (map (lambda (proj)
                               ((proj 'move!)))
                             projectile)
                        ((projectile 'move!))))))
          (if projectile
              ((projectile 'remove-projectile)))))
    
    (define (set-cooldown! num)
      (set! cooldown num))

    (define (set-projectile! value)
      (set! projectile value))

    (define (remove-all-projectiles)
      (if (not (eq? projectile #f))
          (begin
            (map (lambda (proj)
                   ((proj 'remove-projectile)))
                 projectile)
            (set! projectile #f))))

    (define (add-all-projectiles)
      (set! projectile '())
      (define (iter lst)
        (if (not (null? lst))
            (begin
              (let ((proj (make-projectile 'shooter (make-position (position 'get-x) (position 'get-y)) '() dispatch)))
                ((proj 'set-angle!) (car lst))
                (set! projectile (add-element-to-list proj projectile)))
              (iter (cdr lst)))))
      (iter angles)
      (map (lambda (proj)
             ((((environment 'draw) 'projectile-layer) 'add-drawable!) (proj 'get-tile)))
           projectile)
      (set! cooldown cooldown-time))

    (define (remove-projectile-from-list proj)
      (set! projectile (remove-el-from-list proj projectile))
      (if (null? projectile)
          (set! projectile #f)))
  
    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'get-position) position)
            ((eq? mes 'entity?) 'tower);
            ((eq? mes 'check-area) check-area)
            ((eq? mes 'target) target)
            ((eq? mes 'cooldown) cooldown)
            ((eq? mes 'shoot!) shoot!)
            ((eq? mes 'set-cooldown!) set-cooldown!)
            ((eq? mes 'get-projectile) projectile)
            ((eq? mes 'set-projectile!) set-projectile!)
            ((eq? mes 'remove-target) remove-target)
            ((eq? mes 'check-area?) check-area?)
            ((eq? mes 'get-cost) cost)
            ((eq? mes 'get-type) type)
            ((eq? mes 'get-environment) environment)
            ((eq? mes 'remove-all-projectiles) remove-all-projectiles)
            ((eq? mes 'remove-projectile-from-list) remove-projectile-from-list)))
    (set-scale!)
    dispatch))