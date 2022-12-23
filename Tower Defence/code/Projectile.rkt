(#%require "Graphics.rkt")
(load "Position.rkt")

(define (make-projectile position target)
  (let ((tile (make-tile 50 50 "../images/Projectiles/projectile.png" "../images/Projectiles/projectile_mask.png")))

    (define (initiazile-projectile)
      ((tile 'set-x!) (position 'get-x))
      ((tile 'set-y!) (position 'get-y)))
   
    (define (dispatch mes)
      (cond ((eq? mes 'get-tile) tile)
            ((eq? mes 'entity?) 'projectile)
            (else (display "Error: Wrong dispatch message (Projectile.rkt) -> ") (display mes))))
    initiazile-projectile
    dispatch))