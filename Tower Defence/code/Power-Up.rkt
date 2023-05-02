(load "Procedures.rkt")

(define (make-power-up type path . dummy?)
  (let ((tile '())
        (portal-copy '())
        (time 5000)
        (position portal-pos))
    
    (case type
      ("bomb")
      ("portal" (begin
                  (set! tile (make-tile image-size image-size portal-img portal-maks))
                  (if (null? dummy?)
                      (let* ((random-pos (pick-random-from-list (path 'path-positions) start-position end-position))
                             (next-pos ((path 'next-path-to-pos) random-pos)))
                        (set! position (random-pos-between-points random-pos next-pos))
                        (display "X: ")
                        (display (position 'get-x))
                        (display "Y: ")
                        (display (position 'get-y))
                        (newline)))))
      (else "Wrong type selected!"))

    (define (set-scale!) ;This sets the scale of the tower
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'entity?) 'power-up)
            ((eq? mes 'get-type) type)
            (else (display "Error: Wrong dispatch message (Power-Up.rkt) -> ") (display mes))))
    (set-scale!)
    dispatch))