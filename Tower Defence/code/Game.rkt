(load "Tower.rkt")
(load "Monster.rkt")

(define (make-game environment wave player)

  (let ((monster-spawn-time 0)
         (monster-move-time 0))
  
    (define (start!)
      (((environment 'draw) 'draw-start-text))
      ((((environment 'draw) 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if (and (eq? type 'pressed) (eq? key #\s))
             (clean!)))))

    (define (clean!)
      ((((environment 'draw) 'get-text-layer) 'remove-drawable!) ((environment 'draw) 'get-start-text))
      (load-world!)
      ((environment 'remove-all-objects!))
      (set! monster-spawn-time 0)
      (set! monster-move-time 0)
      ((environment 'set-monsters!) '())
      ((environment 'set-towers!) '())
      ((wave 'set-wave!) 0)
      ((wave 'set-wave-list!) '())
      ((wave 'load-wave!))
      (game-loop))

    (define (game-loop) ;This starts the game loop
      ((((environment 'draw) 'get-window) 'set-update-callback!)
       (lambda (ms)
         (set! monster-spawn-time (+ monster-spawn-time ms))
         (set! monster-move-time (+ monster-move-time ms)); Add the extra elapsed time to time-elapsed
         (time-handler ms))) ;Call the time handler to trigger event on certain elapsed time

      ((((environment 'draw) 'get-window) 'set-mouse-click-callback!) ;Reacts and acts upon mouse clicks
       (lambda (button status x y)
         (if (and (eq? button 'left)
                  (eq? status 'pressed))
             (let ((tower (make-tower 4 (make-position (- x (- (modulo x (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50)))) (- y (- (modulo y (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50))))) environment)))
               ((environment 'add-entity!) tower)))))
      
      ((((environment 'draw) 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if (and (eq? type 'pressed) (eq? key #\space))
             (begin (display "Restart") (clean!))))))
  
    (define (load-world!)
      ((environment 'draw) 'draw-world!)) ;Call draw-world! from Draw ADT

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