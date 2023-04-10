(load "Constants.rkt")

(define (make-monster position)
  (let ((tile (make-tile 50 50 "../images/Monsters/monster1.png"))
         (health 1)
         (angle 0))
  
    (define (set-scale!)
      ((tile 'set-scale!) size-factor)

      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (endpoint?)
      ((position 'close-enough?) end-position))

    (define (set-next-position!)
      ((position 'change-coordinates!) (+ (position 'get-x) (* 2 (cos angle))) (+ (position 'get-y) (* 2 (sin angle)))))

    (define (hit!)
      (set! health (- health 1)))

    (define (set-angle! value)
      (set! angle value))
  
    (define (dispatch mes)
      (cond ((eq? mes 'get-position) position)
            ((eq? mes 'get-tile) tile)
            ((eq? mes 'get-increment-x) increment-x)
            ((eq? mes 'get-increment-y) increment-y)
            ((eq? mes 'set-increment!) set-increment!)
            ((eq? mes 'endpoint?) endpoint?)
            ((eq? mes 'entity?) 'monster)
            ((eq? mes 'set-next-position!) (set-next-position!))
            ((eq? mes 'get-health) health)
            ((eq? mes 'set-angle!) set-angle!)
            ((eq? mes 'hit!) hit!)))
    (set-scale!)
    dispatch))