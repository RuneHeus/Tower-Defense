(load "Tower.rkt")
(load "Monster.rkt")

(define (make-game environment wave player draw)

  (let ((monster-spawn-time 0)
        (monster-move-time 0)
        (game-loop-status #t))
  
    (define (start!)
      ((draw 'draw-start-text))
      (((draw 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if (and (eq? type 'pressed) (eq? key #\s))
             (clean!)))))

    (define (clean!)
      ((draw 'remove-previous-text!))
      ((draw 'remove-game-over-screen!))
      (set! monster-spawn-time 0)
      (set! monster-move-time 0)
      ((player 'reset!))
      ((environment 'clean-environment!))
      ((draw 'remove-start-text!))
      ((wave 'start-wave!))
      ((draw 'draw-game-status-text))
      (load-world!)
      (game-loop))

    (define (stop!)
      ((draw 'draw-game-over-screen!))
      (((draw 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if (and (eq? type 'pressed) (eq? key #\s))
             (begin
               (set! game-loop-status #t)
               (clean!))))))

    (define (game-loop);This starts the game loop
      (((draw 'get-window) 'set-update-callback!)
       (lambda (ms)
         (if game-loop-status
             (begin
               (if (<= (player 'get-health) 0)
                   (begin
                     (stop!)
                     (set! game-loop-status #f)))
               (set! monster-spawn-time (+ monster-spawn-time ms))
               (set! monster-move-time (+ monster-move-time ms)); Add the extra elapsed time to time-elapsed
               (time-handler ms))))) ;Call the time handler to trigger event on certain elapsed time

      (((draw 'get-window) 'set-mouse-click-callback!) ;Reacts and acts upon mouse clicks
       (lambda (button status x y)
         (if game-loop
             (if (and (eq? button 'left)
                      (eq? status 'pressed))
                 (begin
                   (if (null? (player 'get-tower-selected))
                       (map (lambda (item)
                              (if (((cdr item) 'equal?) (make-position (- x (- (modulo x (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50)))) (- y (- (modulo y (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50))))))
                                  ((player 'set-selected-tower!) (car item))))
                            menu-positions)
                   
                       (if (>= (player 'get-points) ((player 'get-tower-selected) 'get-cost))
                           (begin
                             ((player 'remove-points!) ((player 'get-tower-selected) 'get-cost))
                             ((draw 'draw-game-status-text))
                             (let ((tower (make-tower ((player 'get-tower-selected) 'get-type) (make-position (- x (- (modulo x (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50)))) (- y (- (modulo y (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50))))) environment)))
                               ((environment 'add-entity!) tower)
                               ((player 'set-selected-tower!) '())))
                           (begin (display "Not enough points!") (newline)))))))))
      
      (((draw 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if game-loop
             (if (and (eq? type 'pressed) (eq? key #\space))
                 (begin ((draw 'draw-game-status-text)) (clean!)))))))
  
    (define (load-world!)
      (draw 'draw-world!)) ;Call draw-world! from Draw ADT

    (define (time-handler ms)
      (if (>= monster-spawn-time 1000)
          (begin (set! monster-spawn-time 0)
                 ((wave 'next-monster!))
                 ((environment 'monster-random-event))))
      (if (>= monster-move-time 10) ;Only move monster each 10 ms, so that it runs sort of even on every computer
          (begin (set! monster-move-time 0)
                 ((environment 'monsters-loop) ms)
                 ((environment 'towers-loop) ms))))
  
    (define (dispatch mes)
      (cond ((eq? mes 'start!) (start!))
            ((eq? mes 'clean!) clean!)
            ((eq? mes 'load-world!) (load-world!))
            (else (display "Error: Wrong dispatch message (Game.rkt): ") (display mes))))
    dispatch))