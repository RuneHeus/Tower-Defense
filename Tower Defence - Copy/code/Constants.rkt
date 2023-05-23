(#%require "Graphics.rkt")
(load "Position.rkt")
(load "Tower.rkt")
(load "Power-Up.rkt")

;--------------------------UI--------------------------------------------
(define width 800) ;width of the window
(define height 600) ;Height of the window

(define size-factor (exact->inexact (/ width 800)));The grid division of the UI
;------------------------------------------------------------------------


;--------------------------Path-position---------------------------------
(define start-position (make-position 0 (* 100 size-factor)))
(define path-1 (make-position (* size-factor 300) (* size-factor 100)))
(define path-2 (make-position (* size-factor 300) (* size-factor 500)))
(define end-position (make-position (* 750 size-factor) (* size-factor 500)))
;------------------------------------------------------------------------

;--------------------------Image-----------------------------------------
(define image-size 50)
(define explosion-size 250)
;------------------------------------------------------------------------

;--------------------------Monster---------------------------------------
(define red-monster-img "../images/Monsters/red-monster.png")
(define red-monster-mask "../images/Monsters/red-monster_mask.png")
(define blue-monster-img "../images/Monsters/blue-monster.png")
(define blue-monster-mask "../images/Monsters/blue-monster_mask.png")
(define gray-monster-img "../images/Monsters/gray-monster.png")
(define gray-monster-mask "../images/Monsters/gray-monster_mask.png")
(define purple-monster-img "../images/Monsters/purple-monster.png")
(define purple-monster-mask "../images/Monsters/purple-monster_mask.png")

(define infection-duration 3000)
;------------------------------------------------------------------------

;--------------------------Projectile-----------------------------------
(define projectile-image-size 25) ;Projectile image size is always 1:1
(define net-image-size 50)
(define standard-projectile-img "../images/Projectiles/projectile.png")
(define standard-projectile-mask "../images/Projectiles/projectile_mask.png")
(define net-projectile-img "../images/Projectiles/net.png")
(define net-projectile-mask "../images/Projectiles/net_mask.png")
(define shooter-projectile-img "../images/Projectiles/shooter.png")
(define shooter-projectile-mask "../images/Projectiles/shooter_mask.png")
;------------------------------------------------------------------------

;--------------------------Tower-----------------------------------------
(define standard-tower-img "../images/Towers/tower1.png")
(define standard-tower-mask "../images/Towers/tower1_mask.png")
(define net-tower-img "../images/Towers/tower4.png")
(define net-tower-mask "../images/Towers/tower4_mask.png")
(define bullet-tower-img "../images/Towers/tower5.png")
(define bullet-tower-mask "../images/Towers/tower5_mask.png")
(define bom-tower-img "../images/Towers/tower6.png")
(define bom-tower-mask "../images/Towers/tower6_mask.png")
;------------------------------------------------------------------------

;--------------------------Power-Up--------------------------------------
(define portal-img "../images/Power-ups/portal.png")
(define portal-img-50 "../images/Power-ups/portal-50.png")
(define portal-mask "../images/Power-ups/portal_mask.png")
(define portal-mask-50 "../images/Power-ups/portal-50_mask.png")
(define portal-copy-img "../images/Power-ups/portal-alt.png")
(define portal-copy-maks "../images/Power-ups/portal-alt_mask.png")
(define bomb-img "../images/Power-ups/bomb.png")
(define bomb-img-50 "../images/Power-ups/bomb-50.png")
(define bomb-mask "../images/Power-ups/bomb_mask.png")
(define bomb-mask-50 "../images/Power-ups/bomb-50_mask.png")
(define explosion-img "../images/Power-ups/explosion.png")
(define explosion-mask "../images/Power-ups/explosion_mask.png")
(define explosion-range 3)
;------------------------------------------------------------------------

;--------------------------Menu--------------------------------------------------------------------------

;--------------tower1-------------------------------------------------------------------
(define tower1-pos (make-position 650 150))
(define tower1 (make-tower 1 tower1-pos "Dummy environment"))
(define cost-text1 (make-tile width height))
;((cost-text1 'draw-text!) (number->string (tower1 'get-cost)) (* size-factor 10) (* size-factor 660) (* size-factor 200) "white")
;----------------------------------------------------------------------------------------

;--------------tower4-------------------------------------------------------------------
(define tower4-pos (make-position 700 150))
(define tower4 (make-tower 4 tower4-pos "Dummy environment"))
(define cost-text4 (make-tile width height))
;((cost-text4 'draw-text!) (number->string (tower4 'get-cost)) (* size-factor 10) (* size-factor 710) (* size-factor 200) "white")
;----------------------------------------------------------------------------------------

;--------------tower5-------------------------------------------------------------------
(define tower5-pos (make-position 650 200))
(define tower5 (make-tower 5 tower5-pos "Dummy environment"))
(define cost-text5 (make-tile width height))
;((cost-text5 'draw-text!) (number->string (tower5 'get-cost)) (* size-factor 10) (* size-factor 660) (* size-factor 270) "white")
;----------------------------------------------------------------------------------------

;--------------tower6-------------------------------------------------------------------
(define tower6-pos (make-position 700 200))
(define tower6 (make-tower 6 tower6-pos "Dummy environment"))
(define cost-text6 (make-tile width height))
;((cost-text6 'draw-text!) (number->string (tower6 'get-cost)) (* size-factor 10) (* size-factor 710) (* size-factor 270) "white")
;----------------------------------------------------------------------------------------

;--------------portal-------------------------------------------------------------------
(define portal-pos (make-position 650 300))
(define portal (make-power-up 'portal "Dummy path" "Dummy environment" "Dummy")) ;This is a dummy portal
;----------------------------------------------------------------------------------------

;--------------bomb-------------------------------------------------------------------
(define bomb-pos (make-position 700 300))
(define bomb (make-projectile 'bomb bomb-pos 'Dummy-target 'Dummy-tower)) ;This is a dummy bomb
;---------------------------------------------------------------------------------------

(define menu-list (list tower1 tower4 tower5 tower6 portal bomb))
(define menu-cost (list cost-text1 cost-text4 cost-text5 cost-text6))
(define menu-positions (list (cons tower1 tower1-pos)
                             (cons tower4 tower4-pos)
                             (cons tower5 tower5-pos)
                             (cons tower6 tower6-pos)
                             (cons portal portal-pos)
                             (cons bomb bomb-pos)))
;-------------------------------------------------------------------------------------------------------

;--------------tower5-angles-----------------------------------------------------------------------------------------------------------------------------------
(define angles (list -1.5707963267948966 -0.7853981633974483 0 0.7853981633974483 1.5707963267948966 3.9269908169872414 3.141592653589793 -3.9269908169872414))
;--------------------------------------------------------------------------------------------------------------------------------------------------------------