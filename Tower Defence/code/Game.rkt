(load "Tower.rkt")
(load "Monster.rkt")
(#%require (only racket/base random))

(define (make-game environment wave player draw path)

  (let ((monster-spawn-time 0)
        (monster-move-time 0)
        (game-loop-status #t)
        (menu-clicked? #f))
  
    (define (start!)
      ((draw 'draw-start-text))
      (((draw 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if (and (eq? type 'pressed) (eq? key #\s))
             (begin
               (load-world!)
               (clean!)
               (game-loop))))))

    (define (reset!)
      (set! monster-spawn-time 0)
      (set! monster-move-time 0))
    
    (define (clean!)
      (reset!)
      ((player 'reset!))
      ((environment 'reset!))
      ((draw 'reset!))
      ((wave 'reset!))
      )

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
               (time-handler ms)
               )))) ;Call the time handler to trigger event on certain elapsed time

      (((draw 'get-window) 'set-mouse-click-callback!) ;Reacts and acts upon mouse clicks
       (lambda (button status x y)
         (if game-loop-status
             (begin 
               (set! menu-clicked? #f)
               (if (and (eq? button 'left)
                        (eq? status 'pressed))
                   (begin
                     (map (lambda (item)
                            (if (((cdr item) 'equal?) (make-position (- x (- (modulo x (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50)))) (- y (- (modulo y (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50))))))
                                (menu-click item)))
                          menu-positions)
                     (if (not menu-clicked?)
                         (if (not (null? (player 'get-tower-selected)))
                             (if (>= (player 'get-points) ((player 'get-tower-selected) 'get-cost))
                                 (begin
                                   ((player 'remove-points!) ((player 'get-tower-selected) 'get-cost))
                                   ((draw 'draw-game-status-text))
                                   (let ((tower (make-tower ((player 'get-tower-selected) 'get-type) (make-position (- x (- (modulo x (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50)))) (- y (- (modulo y (ceiling (* size-factor 50))) (- (ceiling (* size-factor 50)) (* size-factor 50))))) environment)))
                                     ((environment 'add-entity!) tower)
                                     ((player 'set-selected-tower!) '())))
                                 (begin (display "Not enough points!") (newline)))))))))))
      
      (((draw 'get-window) 'set-key-callback!)
       (lambda (type key)
         (if game-loop
             (begin 
               (if (and (eq? type 'pressed) (eq? key #\space))
                   (begin ((draw 'draw-game-status-text)) (clean!) ((draw 'remove-wave-count!))))
               (if (and (eq? type 'pressed) (eq? key #\w))
                   (if (wave 'wave-ready?)
                       (begin ((wave 'load-wave!)) ((draw 'remove-wave-text!)))))
               (if (and (eq? type 'pressed) (eq? key #\m)) ((player 'set-points!) 99999999) ((draw 'draw-game-status-text))))))))
  
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
                 (if (not (wave 'wave-ready?))
                     ((environment 'towers-loop) ms))
                 ((environment 'obstacle-process) ms)
                 (player-process ms))))

    (define (player-process ms)
      (if (not (null? (player 'get-portal-timer)))
          (if (<= (player 'get-portal-timer) 0)
              (begin
                ((player 'set-portal-time!) '())
                ((draw 'remove-portal-opacity!)))
              ((player 'portal-minus-time) ms)))
      (if (not (null? (player 'get-bomb-timer)))
          (if (<= (player 'get-bomb-timer) 0)
              (begin
                ((player 'set-bomb-time!) '())
                ((draw 'remove-bomb-opacity!)))
              ((player 'bomb-minus-time) ms))))

    (define (menu-click item)
      (begin
        (set! menu-clicked? #t)
        (if (eq? ((car item) 'entity?) 'power-up)
            (if (eq? ((car item) 'get-type) 'bomb)
                (if (null? (player 'get-bomb-timer))
                    (begin
                      ((player 'set-bomb-time!) 10000)
                      ((draw 'add-bomb-opacity!))
                      (let ((rand-amount (random 1 5))
                            (projectile '()))
                        (do ((i 0 (+ i 1)))
                          ((= i rand-amount))
                          (begin
                            (set! projectile (make-projectile ((car item) 'get-type) ((path'random-pos-on-path)) '() '()))
                            ((projectile 'set-projectile?) #f)
                            ((environment 'add-obstacle) projectile))))))
                (if (null? (player 'get-portal-timer))
                    (begin 
                      ((player 'set-portal-time!) 10000)
                      ((draw 'add-portal-opacity!))
                      ((environment 'add-obstacle) (make-power-up ((car item) 'get-type) path)))))
            ((player 'set-selected-tower!) (car item)))))
   
    (define (dispatch mes)
      (cond ((eq? mes 'start!) (start!))
            ((eq? mes 'clean!) clean!)
            ((eq? mes 'load-world!) (load-world!))
            (else (display "Error: Wrong dispatch message (Game.rkt): ") (display mes))))
    dispatch))