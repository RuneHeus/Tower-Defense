(load "Constants.rkt")

(define (make-monster type position)
  (let ((tile (make-tile 50 50 "../images/Monsters/red-monster.png" "../images/Monsters/red-monster_mask.png"))
         (health 1)
         (angle 0)
         (speed 1)
         (last-path-position '()))
    
  (case type ;If red is chosen then no option is selected, then we use the default values
    ("Blue" (begin
              (set! health 3)
              (set! speed health)
              (set! tile (make-tile 50 50 "../images/Monsters/blue-monster.png" "../images/Monsters/blue-monster_mask.png")))))
  
  
    (define (set-scale!)
      ((tile 'set-scale!) size-factor)

      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (endpoint?)
      ((position 'close-enough?) end-position))

    (define (set-next-position!)
      ((position 'change-coordinates!) (+ (position 'get-x) (round (* speed (cos angle)))) (+ (position 'get-y) (round (* speed (sin angle))))))

    (define (hit!)
      (set! health (- health 1)))

    (define (set-angle! value)
      (set! angle value))

    (define (set-position! pos)
      (set! position pos))

    (define (set-last-path-position! position)
      (set! last-path-position position))
  
    (define (dispatch mes)
      (cond ((eq? mes 'get-position) position)
            ((eq? mes 'set-position!) set-position!)
            ((eq? mes 'get-tile) tile)
            ((eq? mes 'get-increment-x) increment-x)
            ((eq? mes 'get-increment-y) increment-y)
            ((eq? mes 'set-increment!) set-increment!)
            ((eq? mes 'endpoint?) endpoint?)
            ((eq? mes 'entity?) 'monster)
            ((eq? mes 'set-next-position!) (set-next-position!))
            ((eq? mes 'get-health) health)
            ((eq? mes 'set-angle!) set-angle!)
            ((eq? mes 'get-angle) angle)
            ((eq? mes 'get-speed) speed)
            ((eq? mes 'hit!) hit!)
            ((eq? mes 'set-last-path-position!) set-last-path-position!)
            ((eq? mes 'get-last-path-position) last-path-position)))
    (set-scale!)
    dispatch))