;; This is a "Sandbox", Where I can safely test parts of the code of the project
;
(#%require "Graphics.rkt")
;(load "Constants.rkt")
;(load "Monster.rkt")
;(load "Projectile.rkt")
;(load "Tower.rkt")
;
;(define size-factor (exact->inexact (/ width 800)))
;
;(define window (make-window width height "Test"))
;
;(define (draw-world!)
;    (let ((background-layer ((window 'new-layer!)))
;          (tile (make-tile width height)))
;      ((tile 'draw-rectangle!) 0 0 width height "green")
;      ((tile 'draw-rectangle!) 0 (* 100 size-factor) (* 300 size-factor) (* 50 size-factor) "brown") ;Pad ((0,5) -> (15,5)) (division)
;      ((tile 'draw-rectangle!) (* 300 size-factor) (* 100 size-factor) (* size-factor 50) (* 400 size-factor) "brown") ;Pad ((15,5) -> (15,35)) (division)
;      ((tile 'draw-rectangle!) (* size-factor 300) (* size-factor 500) (* 500 size-factor)  (* 50 size-factor) "brown") ;Pad ((15,35) -> (40,35)) (division)
;      ((background-layer 'add-drawable!) tile)))
;
;(draw-world!)
;
;(define layer ((window 'new-layer!)))
;
;(define monster (make-monster start-position))
;
;((layer 'add-drawable!) (monster 'get-tile))
;
;(define pos-layer ((window 'new-layer!)))
;
;(define time-elapsed 0)
;
;(((monster 'get-tile) 'set-scale!) size-factor)
;
;(((monster 'get-tile) 'set-x!) (+ (- (/ (* ((monster 'get-tile) 'get-w) size-factor) 2) (/ ((monster 'get-tile) 'get-w) 2)) ((monster 'get-position) 'get-x)))
;(((monster 'get-tile) 'set-y!) (+ (- (/ (* ((monster 'get-tile) 'get-h) size-factor) 2) (/ ((monster 'get-tile) 'get-h) 2)) ((monster 'get-position) 'get-y)))
;
;(define (draw-stripes-horizontal!)
;  (let loop ((pos-tile (make-tile width height))
;             (y 0))
;    (cond ((> y height) ((pos-layer 'add-drawable!) pos-tile))
;          (else
;           (begin
;             ((pos-tile 'draw-line!) 0 y width y 1 "black")
;             (loop pos-tile (+ y (* 50 size-factor))))))))
;
;(define (draw-stripes-vertical!)
;  (let loop ((pos-tile (make-tile width height))
;             (x 0))
;    (cond ((> x width) ((pos-layer 'add-drawable!) pos-tile))
;          (else
;           (begin
;             ((pos-tile 'draw-line!) x 0 x height 1 "black")
;             (loop pos-tile (+ x (* 50 size-factor))))))))
;                                      
;(draw-stripes-horizontal!)
;(draw-stripes-vertical!)
;
;((window 'set-mouse-click-callback!)
;     (lambda (button status x y)
;       (if (and (eq? button 'left)
;                (eq? status 'pressed))
;           (let* ((tower (make-tile 50 50 "../images/Towers/tower1.png" "../images/Towers/tower1_mask.png"))
;                  (projectile (make-projectile (make-position (tower 'get-x) (tower 'get-y)) #f)))
;             ((tower 'set-x!) (- x (modulo x 50)))
;             ((tower 'set-y!) (- y (modulo y 50)))
;             
;             ((layer 'add-drawable!) tower)))))
(generate-mask "../images/Projectiles/projectile.png" "white")