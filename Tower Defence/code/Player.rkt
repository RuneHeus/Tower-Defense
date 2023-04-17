(define (make-player)
  (let ((health 10)
        (coins 0)
        (tower-selected '())
        (power-up-selected '()))

    (define (set-health! value)
      (set! health value))

    (define (damage! monster)
      (set-health! (- health (monster 'get-damage))))

    (define (set-selected-tower! tower)
      (set! tower-selected tower))
    
    (define (set-power-up-selected! power-up)
      (set! power-up-selected power-up))

    (define (remove-coins! amount)
      (set! coins (- coins amount)))

    (define (add-coins! amount)
      (set! coins (+ coins amount)))

    (define (dispatch mes)
      (cond ((eq? mes 'set-health!) set-health!)
            ((eq? mes 'damage!) damage!)
            ((eq? mes 'set-selected-tower!) set-selected-tower!)
            ((eq? mes 'set-power-up-selected!) set-power-up-selected!)
            ((eq? mes 'get-health) health)
            ((eq? mes 'get-tower-selected) tower-selected)
            ((eq? mes 'get-power-up-selected) power-up-selected)
            ((eq? mes 'remove-coins!) remove-coins!)
            ((eq? mes 'add-coins!) add-coins!)
            (else (display "Error: Wrong dispatch message (Player.rkt) -> ") (display mes))))
    dispatch))