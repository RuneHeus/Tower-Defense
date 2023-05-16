(load "Procedures.rkt")

(define (make-power-up type path . dummy?)
  (let ((tile '())
        (portal-copy (make-tile image-size image-size portal-copy-img portal-copy-maks))
        (timer 5000)
        (position '())
        (behaviour '()))
    
    (case type
      ('portal (begin
                  (set! tile (make-tile image-size image-size portal-img portal-mask))
                  (set! position portal-pos)
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
                                    ))))
      (else "Wrong type selected!"))

    (define (set-scale!) ;This sets the scale of the tower
      ((tile 'set-scale!) size-factor)
      ((tile 'set-x!) (+ (- (/ (* (tile 'get-w) size-factor) 2) (/ (tile 'get-w) 2)) (position 'get-x)))
      ((tile 'set-y!) (+ (- (/ (* (tile 'get-h) size-factor) 2) (/ (tile 'get-h) 2)) (position 'get-y))))

    (define (set-tile! new)
      (set! tile new))

    (define (set-timer! time)
      (set! timer time))

    (define (minus-time! amount)
      (set! timer (- timer amount)))

    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'set-tile!) set-tile!)
            ((eq? mes 'entity?) 'power-up)
            ((eq? mes 'get-type) type)
            ((eq? mes 'get-position) position)
            ((eq? mes 'get-behaviour) behaviour)
            ((eq? mes 'get-portal-copy) portal-copy)
            ((eq? mes 'get-timer) timer)
            ((eq? mes 'set-timer!) set-timer!)
            ((eq? mes 'minus-time!) minus-time!)
            (else (display "Error: Wrong dispatch message (Power-Up.rkt) -> ") (display mes))))
    (set-scale!)
    dispatch))