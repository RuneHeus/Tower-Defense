(#%require "Graphics.rkt")
(load "Environment.rkt")
(load "Game.rkt")
(load "Path.rkt")
(load "Constants.rkt")
(load "Draw.rkt")
(load "Wave.rkt")
(load "Player.rkt")
(load "Procedures.rkt")

;All the pre initialisation that is needed to start the game
(define Path (make-path start-position end-position))

(define Draw (make-draw))

(define Player (make-player))

(define Environment (make-environment Draw Path Player))

(define Wave (make-wave Environment))

(define Game (make-game Environment Wave Player))

(Game 'start!) ;Start the game