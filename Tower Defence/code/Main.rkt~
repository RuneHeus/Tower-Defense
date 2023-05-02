(#%require "Graphics.rkt")
(load "Environment.rkt")
(load "Game.rkt")
(load "Path.rkt")
(load "Constants.rkt")
(load "Draw.rkt")
(load "Wave.rkt")
(load "Player.rkt")
(load "Procedures.rkt")

;All the pre initialisation that are needed to start the game
(define Path (make-path start-position end-position))

(define Player (make-player))

(define Draw (make-draw player))

(define Environment (make-environment Draw Path Player))

(define Wave (make-wave Environment))

(define Game (make-game Environment Wave Player Draw))

(Game 'start!) ;Start the game