(define (make-player)
  (let ((health 10)
        (points 200)
        (tower-selected '())
        (portal-timer '())
        (bomb-timer '()))

    (define (set-health! value)
      (set! health value))

    (define (damage! monster)
      (set-health! (- health (monster 'get-damage))))

    (define (set-selected-tower! tower)
      (set! tower-selected tower))

    (define (remove-points! amount)
      (set! points (- points amount)))

    (define (add-points! amount)
      (set! points (+ points amount)))

    (define (set-points! value)
      (set! points value))

    (define (reset!)
      (set-health! 10)
      (set-points! 200)
      (set-selected-tower! '())
      (set! portal-timer '())
      (set! bomb-timer '()))

    (define (portal-minus-time time)
      (set! portal-timer (- portal-timer time)))

    (define (set-portal-time! time)
      (set! portal-timer time))

    (define (bomb-minus-time time)
      (set! bomb-timer (- bomb-timer time)))

    (define (set-bomb-time! time)
      (set! bomb-timer time))

    (define (dispatch mes)
      (cond ((eq? mes 'set-health!) set-health!)
            ((eq? mes 'damage!) damage!)
            ((eq? mes 'set-selected-tower!) set-selected-tower!)
            ((eq? mes 'get-health) health)
            ((eq? mes 'get-tower-selected) tower-selected)
            ((eq? mes 'get-power-up-selected) power-up-selected)
            ((eq? mes 'remove-points!) remove-points!)
            ((eq? mes 'add-points!) add-points!)
            ((eq? mes 'get-points) points)
            ((eq? mes 'reset!) reset!)
            ((eq? mes 'set-points!) set-points!)
            ((eq? mes 'portal-minus-time) portal-minus-time)
            ((eq? mes 'set-portal-time!) set-portal-time!)
            ((eq? mes 'bomb-minus-time) bomb-minus-time)
            ((eq? mes 'set-bomb-time!) set-bomb-time!)
            ((eq? mes 'get-portal-timer) portal-timer)
            ((eq? mes 'get-bomb-timer) bomb-timer)
            (else (display "Error: Wrong dispatch message (Player.rkt) -> ") (display mes))))
    dispatch))