(#%require (only racket/base sleep))

(define (make-environment draw path player)

  (let ((monsters '())
        (towers '())
        (obstacles '()))
    
    (define (add-entity! entity)
      (cond ((eq? (entity 'entity?) 'monster) ;If the entity is equal to a monster
             (if (null? monsters)
                 (set! monsters (list entity))
                 (set! monsters (append monsters (list entity))))
             (draw-entity! entity))
            ((eq? (entity 'entity?) 'tower)
             (if (and (free-position? entity) (not ((path 'on-path?) entity)))
                 (begin (if (null? towers)
                            (set! towers (list entity))
                            (set! towers (append towers (list entity))))
                        (draw-entity! entity))
                 (display "Unable to place tower at this position")))))

    (define (draw-entity! entity)
      ((draw 'draw!) (entity 'get-tile)))

    (define (set-monsters! val)
      (set! monsters val))
  
    (define (move-monster! monster)
      (monster 'set-next-position!)
      ((draw 'reposition!) (monster 'get-tile) (monster 'get-position)))

    (define (remove-monster! monster reason)
      (if (eq? reason "End")
          ((player 'damage!) monster))
      (define (remove list) ;Iterative searching for monster
        (if (null? list)
            '()
            (if (equal? (car list) monster) ;If object is equal to monster
                (remove (cdr list)); leave it out of the list
                (cons (car list) (remove (cdr list)))))) ;Move on with next monster
      (set-monsters! (remove monsters))
      (((draw 'entity-layer) 'remove-drawable!) (monster 'get-tile)) ;Remove monster drawable from screen
      (map (lambda (tower)
             ((tower 'remove-target) monster)) towers))

    (define (remove-all-monsters!)
      (map
       (lambda (monster)
         (((draw 'entity-layer) 'remove-drawable!) (monster 'get-tile)))
       monsters)
      (set! monsters '()))

    (define (remove-all-towers!)
      (map
       (lambda (tower)
         (((draw 'entity-layer) 'remove-drawable!) (tower 'get-tile)))
       towers)
      (set! towers '()))

    (define (remove-all-projectiles!)
      (map
       (lambda (tower)
         (if (tower 'get-projectile)
             (((draw 'projectile-layer) 'remove-drawable!) ((tower 'get-projectile) 'get-tile))))
       towers))

    (define (remove-all-obstacles!)
      (map
       (lambda (obstacle)
         (((draw 'entity-layer) 'remove-drawable!) (obstacle 'get-tile)))
       obstacles))

    (define (remove-all-objects!)
      (remove-all-monsters!)
      (remove-all-towers!)
      (remove-all-projectiles!)
      (remove-all-obstacles!))

    (define (clean-environment!)
      (remove-all-objects!)
      (set-monsters! '())
      (set-towers! '())
      (set-obstacles! '()))

      
    (define (set-new-increment! monster)
      (let ((monster-pos (monster 'get-position))
            (move-monster? #t) ;Is the monster allowed to move?
            (next-pos ((path 'next-path-to-pos) (monster 'get-last-path-position))) ;The next path position from the monster
            (obstacle? (object-eq-pos-obstacles? monster obstacles)));Is the monster close enough to a obstacle?
        (define (loop paths) ;Loop through all the path positions
          (if ((monster 'endpoint?))
              (begin (remove-monster! monster "End") ((draw 'draw-game-status-text)))
              (if (not (null? paths))
                  (begin
                    (if obstacle? ;If the monster is at in range of a obstacle
                        ((obstacle? 'get-behaviour) monster))
                    (if ((monster-pos 'equal?) (car paths))
                        (begin 
                          ((monster 'set-angle!) (atan (- ((cadr paths) 'get-y) (monster-pos 'get-y)) (- ((cadr paths) 'get-x) (monster-pos 'get-x))))
                          ((monster 'set-last-path-position!) (car paths)))
                        (if ((path 'next-pos-to-far?) monster next-pos) ;Is the monster going to overshoot the next path position?
                            (begin
                              ((monster-pos 'change-coordinates!) (next-pos 'get-x) (next-pos 'get-y))
                              ((draw 'reposition!) (monster 'get-tile) (monster 'get-position))
                              (set! move-monster? #f))))
                    (loop (cdr paths))))))
        (loop (path 'path-positions))
        move-monster?))
  
    (define (monsters-loop ms)
      (map (lambda (monster)
             (if (<= (monster 'get-health) 0)
                 (begin
                   (case (monster 'get-type)
                     ("Purple" ((monster 'on-death) monsters (next-pos-path monster)))
                     ("Gray" (add-entity! (make-monster "Red" (make-position (start-position 'get-x) (start-position 'get-y))))
                             (add-entity! (make-monster "Red" (make-position (start-position 'get-x) (start-position 'get-y))))))
                   (remove-monster! monster "Death")
                   ((player 'add-points!) (monster 'get-points))
                   ((draw 'draw-game-status-text)))
                 (begin
                   (if (set-new-increment! monster) ;Calculates the way it has to move
                       (move-monster! monster))
                   (if (monster 'get-infection)
                       (if (<= (monster 'get-infection) 0)
                           (begin
                             ((monster 'set-speed!) (monster 'get-default-speed))
                             ((monster 'set-infection!) #f))
                           ((monster 'set-infection!) (- (monster 'get-infection) ms)))))))
           monsters))

    (define (free-position? tower) ;Checks if there already is a tower in the position
      (let loop ((lijst towers))
        (cond ((null? lijst) #t)
              ((or ((((car lijst) 'get-position) 'equal?) (tower 'get-position)) ((path 'on-path?) tower)) #f)
              (else (loop (cdr lijst))))))
  
    (define (on-path? tower)
      (let loop ((lijst (path 'path-positions))
                 (tow-pos (tower 'get-position)))
        (cond ((null? (cdr lijst)) #f)
              ((and (= (tow-pos 'get-x) ((car lijst) 'get-x)) (= (tow-pos 'get-x) ((cadr lijst) 'get-x)))
               (and (>= (tow-pos 'get-y) ((car lijst) 'get-y)) (<= (tow-pos 'get-y) ((cadr lijst) 'get-y))))
              ((and (= (tow-pos 'get-y) ((car lijst) 'get-y)) (= (tow-pos 'get-y) ((cadr lijst) 'get-y)))
               (and (>= (tow-pos 'get-x) ((car lijst) 'get-x)) (<= (tow-pos 'get-x) ((cadr lijst) 'get-x))))
              (else (loop (cdr lijst) tow-pos)))))

    (define (add-obstacle obstacle)
      (if (null? obstacles)
          (set! obstacles (list obstacle))
          (set! obstacles (append obstacles (list obstacle))))
      (((draw 'get-power-up-layer) 'add-drawable!) (obstacle 'get-tile)))
    
    (define (check-new-obstacles-tower)
      (map (lambda (tower)
             (let ((projectile (tower 'get-projectile)))
               (if projectile
                   (if (projectile 'obstacle?)
                       (if (not (memq projectile obstacles))
                           (add-obstacle projectile))))))
           towers))
    
    (define (towers-loop ms)
      (map (lambda (tower)
             (towers-shoot tower)
             (calculate-cooldown tower ms)
             (check-tower-areas tower)
             (if (tower 'check-area?)
                 (check-new-obstacles-tower)))
           towers))

    (define (set-towers! val)
      (set! towers val))

    (define (towers-shoot tower)
      ((tower 'shoot!)))
  
    (define (check-tower-areas tower)
      (map (lambda (monster)
             ((tower 'check-area) monster))
           monsters))

    (define (calculate-cooldown tower ms)
      (if (not (= (tower 'cooldown) 0))
          (if (> 0 (- (tower 'cooldown) ms))
              ((tower 'set-cooldown!) 0)
              ((tower 'set-cooldown!) (- (tower 'cooldown) ms))))
      (if (tower 'get-projectile)
          (if (<= ((tower 'get-projectile) 'get-cooldown) 0)
              (((tower 'get-projectile) 'remove-projectile))
              (((tower 'get-projectile) 'minus-cooldown) ms))))

    (define (monster-random-event)
      (map (lambda (monster)
             (if (not (null? (monster 'get-random-event)))
                 ((monster 'get-random-event)))) monsters))

    (define (set-obstacles! val)
      (set! obstacles val))

    (define (dispatch mes)
      (cond ((eq? mes 'draw) draw)
            ((eq? mes 'get-monsters) monsters)
            ((eq? mes 'set-monsters!) set-monsters!)
            ((eq? mes 'set-towers!) set-towers!)
            ((eq? mes 'get-path) path)
            ((eq? mes 'add-entity!) add-entity!)
            ((eq? mes 'monsters-loop) monsters-loop)
            ((eq? mes 'check-tower-areas) check-tower-areas)
            ((eq? mes 'on-path?) on-path?)
            ((eq? mes 'towers-loop) towers-loop)
            ((eq? mes 'draw-entity!) draw-entity!)
            ((eq? mes 'find-tower-by-projectile) find-tower-by-projectile)
            ((eq? mes 'remove-all-objects!) remove-all-objects!)
            ((eq? mes 'monster-random-event) monster-random-event)
            ((eq? mes 'add-obstacle) add-obstacle)
            ((eq? mes 'set-obstacles!) set-obstacles!)
            ((eq? mes 'get-obstacles) obstacles)
            ((eq? mes 'clean-environment!) clean-environment!)
            (else (display "Error: Wrong dispatch message (Environment.rkt) -> ") (display mes))))
    dispatch))