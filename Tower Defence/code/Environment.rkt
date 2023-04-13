(load "Monster.rkt")

(define (make-environment draw path)

  (let ((monsters '())
         (towers '()))
    
    (define (add-entity! entity)
      (cond ((eq? (entity 'entity?) 'monster) ;If the entity is equal to a monster
             (if (null? monsters)
                 (set! monsters (list entity))
                 (set! monsters (append monsters (list entity))))
             (draw-entity! entity))
            ((eq? (entity 'entity?) 'tower)
             (if (and (free-position? entity) (not (on-path? entity)))
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

    (define (remove-monster! monster)
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
       monsters))

    (define (remove-all-towers!)
      (map
       (lambda (tower)
         (((draw 'entity-layer) 'remove-drawable!) (tower 'get-tile)))
       towers))

    (define (remove-all-projectiles!)
      (map
       (lambda (tower)
         (if (tower 'get-projectile)
             (((draw 'projectile-layer) 'remove-drawable!) ((tower 'get-projectile) 'get-tile))))
       towers))

    (define (remove-all-objects!)
      (remove-all-monsters!)
      (remove-all-towers!)
      (remove-all-projectiles!))
  
    (define (set-new-increment! monster)
      (let ((monster-pos (monster 'get-position))
            (move-monster? #t)
            (next-pos (next-pos-path monster)))
        (define (loop paths)
          (if ((monster 'endpoint?))
              (remove-monster! monster)
              (if (not (null? paths))
                  (begin
                    (if ((monster-pos 'equal?) (car paths))
                        (begin 
                          ((monster 'set-angle!) (atan (- ((cadr paths) 'get-y) (monster-pos 'get-y)) (- ((cadr paths) 'get-x) (monster-pos 'get-x))))
                          ((monster 'set-last-path-position!) (car paths)))
                        (if (next-pos-to-far? monster next-pos)
                            (begin
                              ((monster-pos 'change-coordinates!) (next-pos 'get-x) (next-pos 'get-y))
                              ((draw 'reposition!) (monster 'get-tile) (monster 'get-position))
                              (set! move-monster? #f))))
                    (loop (cdr paths))))))
        (loop (path 'path-positions))
        move-monster?))

    (define (next-pos-path monster)
      (define (loop paths)
        (if (not (null? paths))
            (if (((monster 'get-last-path-position) 'equal?) (car paths))
                (car (cdr paths))
                (loop (cdr paths)))
            '()))
      (if (null? (monster 'get-last-path-position))
          (car (path 'path-positions))
          (loop (path 'path-positions))))

    (define (next-pos-to-far? monster position);If the increment for the next position is greater than the position of the path, then the code wont recognize that he passed that position, so with this code it fixes that
      (and (>= (+ ((monster 'get-position) 'get-x) (round (* (monster 'get-speed) (cos (monster 'get-angle))))) (position 'get-x))
           (>= (+ ((monster 'get-position) 'get-y) (round (* (monster 'get-speed) (sin (monster 'get-angle))))) (position 'get-y))))
  
    (define (monsters-loop)
      (map (lambda (monster)
             (if (<= (monster 'get-health) 0)
                 (remove-monster! monster)
                 (begin
                   (if (set-new-increment! monster) ;Calculates the way it has to move
                        (move-monster! monster)))))
           monsters))

    (define (free-position? tower)
      (let loop ((lijst towers))
        (cond ((null? lijst) #t)
              ((or ((((car lijst) 'get-position) 'equal?) (tower 'get-position)) (on-path? tower)) #f)
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

    (define (towers-loop ms)
      (map (lambda (tower)
             (towers-shoot tower)
             (calculate-cooldown tower ms)
             (check-tower-areas tower))
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
              ((tower 'set-cooldown!) (- (tower 'cooldown) ms)))))
  
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
            (else (display "Error: Wrong dispatch message (Environment.rkt) -> ") (display mes))))
    dispatch))