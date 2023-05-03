(load "Procedures.rkt")

(define (make-power-up type path . dummy?)
  (let ((tile '())
        (portal-copy (make-tile image-size image-size portal-copy-img portal-copy-maks))
        (time 5000)
        (position portal-pos)
        (behaviour '()))
    
    (case type
      ("bomb")
      ("portal" (begin
                  (set! tile (make-tile image-size image-size portal-img portal-maks))
                  (if (null? dummy?)
                      (begin
                        (let* ((random-pos (pick-random-from-list (path 'path-positions) start-position end-position))
                                    (next-pos ((path 'next-path-to-pos) random-pos)))
                               (set! position (random-pos-between-points random-pos next-pos)))
                      
                             (let* ((random-pos (pick-random-from-list (list start-position)))
                                    (next-pos ((path 'next-path-to-pos) random-pos))
                                    (calculated-pos (random-pos-between-points random-pos next-pos)))
                               ((portal-copy 'set-x!) (calculated-pos 'get-x))
                               ((portal-copy 'set-y!) (calculated-pos 'get-y)))))
                  (set! behaviour (lambda (monster)
                                    (((monster 'get-position) 'change-coordinates!) (portal-copy 'get-x) (portal-copy 'get-y))
                                    ((monster 'set-last-path-position!) start-position)
                                    ((monster 'set-angle!) (atan (- ((path 'get-path1) 'get-y) ((monster 'get-position) 'get-y)) (- ((path 'get-path1) 'get-x) ((monster 'get-position) 'get-x))))
                                    (((monster 'get-last-path-position) 'display-position))
                                    (display (monster 'get-angle))
                                    ))))
      (else "Wrong type selected!"))

    (define (set-scale!) ;This sets the scale of the tower
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'entity?) 'power-up)
            ((eq? mes 'get-type) type)
            ((eq? mes 'get-position) position)
            ((eq? mes 'get-behaviour) behaviour)
            ((eq? mes 'get-portal-copy) portal-copy)
            (else (display "Error: Wrong dispatch message (Power-Up.rkt) -> ") (display mes))))
    (set-scale!)
    dispatch))