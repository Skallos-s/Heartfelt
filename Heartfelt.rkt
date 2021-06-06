#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require (only-in racket/gui/base get-display-backing-scale))

(define MOVESPEED 4)
(define TIMESPEED 40)
(define ANIMATIONSPEED 0.5)

; ### #   # ####  ##
;  #  ##  # #    #  #
;  #  # # # #### #  #
;  #  # # # #    #  #
;  #  #  ## #    #  #
; ### #   # #     ##
(define SHEETS (list "assets\\.\\sheet1.png" "assets\\.\\sheet2.png" "assets\\.\\sheet3.png"))
(define LEVELS (list "data\\.\\lvl1.dat" "data\\.\\lvl2.dat" "data\\.\\lvl3.dat"))
(define STARTS (list (list 3 14 2) (list 9 15 3) (list 18 10 4)))
(define TITLES (list "Caverns" "Apocalypse" "Laboratory"))
(define THINGS (list 3 4 7))
(define DIALOG (list
                (list "I need to find a way out."
                      "The ground must\nhave collapsed."
                      "Where am I?")
                (list "I should find a way\nto put out the fire."
                      "Lucas' exeriment\nmust have caused this."
                      "What happened?")
                (list "Maybe things will\ngo back to normal."
                      "I have to find a way to\nfix all this."
                      "This is the lab\nwhere Lucas works.")))

; Real Remainder
; Like remainder, but works with reals.
; real real -> real
(define (rremainder n m) (- n (* m (floor (/ n m)))))

;  ### ##### ###  #  #  ##  #####
; #      #   #  # #  # #  #   #
;  #     #   #  # #  # #      #
;   #    #   ###  #  # #      #
;    #   #   #  # #  # #  #   #
; ###    #   #  #  ##   ##    #
; meta-data stores extraneous info, such as collected items, level name, etc...
; (list [LEVEL NAME] [LEVEL NUMBER] [TIME CONSUMED] [ITEMS...])
(define-struct game (position    ; List[0 x : 1 y : 2 t] of Integers
                     health      ; Integer
                     direction   ; Integer
                     meta-data   ; List of [%%%]
                     level-data  ; List[Time][y][x] of Integers
                     sprite-list ; List of Images
                     draw-view   ; List[y][x] of Images
                     buffer-view ; { #f : List[y][x] of Images }
                     anim-timer  ; Integer
                     anim-type)) ; Integer

; #   #  ##  #  # ####
; ## ## #  # #  # #
; # # # #  # # #  ####
; #   # #### ###  #
; #   # #  # #  # #
; #   # #  # #  # ####

; Read Level Data
; Reads in binary data and produces the level map.
; First 3 bytes are the level dimensions.
; Rest is level data to be parsed into triple nested lists.
; Time y x [DATA]
; string -> ListOf[ListOf[ListOf[integer]]]
(define
  (read-level-data FILE)
  (let*
      ([data (bytes->list (port->bytes (open-input-file FILE)))]
       [tco (first data)]
       [yco (second data)]
       [xco (third data)]
       [dat (drop data 3)])
    (for/list ([i (range tco)])
      (for/list ([j (range yco)])
        (for/list ([k (range xco)])
          (list-ref dat (+ k (* xco j) (* xco yco i))))))))

; Make Draw Screen
; Take in new info and generate the next draw screen.
; Also for drawing the buffer.
; ListOf[intger] integer ListOf[Listof[Listof[integer]]] Listof[image] -> ListOf[ListOf[image]]
(define
  (make-draw-screen POSN DIRE DATA SPRI)
  (map
   (lambda (row)
     (map
      (lambda (col)
        (list-ref SPRI col))
      row))
   (let
       ([xpo (first  POSN)]
        [ypo (second POSN)]
        [tpo (third  POSN)])
     (for/list
         ([i (range -4 5)])
       (for/list
           ([j (range -4 5)])
         (cond
           [(= 0 DIRE)
            (with-handlers
                ([exn:fail? (lambda (f) 0)])
              (foldl
               (lambda (x y) (list-ref y x))
               DATA
               (list tpo (+ ypo i) (+ xpo j))))]
           [(= 1 DIRE)
            (with-handlers
                ([exn:fail? (lambda (f) 0)])
              (foldl
               (lambda (x y) (list-ref y x))
               DATA
               (list (+ tpo j) ypo (+ xpo i))))]
           [else
            (with-handlers
                ([exn:fail? (lambda (f) 0)])
              (foldl
               (lambda (x y) (list-ref y x))
               DATA
               (list (+ tpo i) (+ ypo j) xpo)))]))))))

; Merge draw-view with new draw-view for move animation.
; ListOf[ListOf[image]] ListOf[ListOf[image]] integer -> ListOf[ListOf[image]]
(define
  (merge drvi nviw arro)
  (cond
    [(= 0 arro) (append (list (first nviw)) drvi)]
    [(= 1 arro) (map (lambda (x y) (append x (list (last y)))) drvi nviw)]
    [(= 2 arro) (append drvi (list (last nviw)))]
    [else (map (lambda (x y) (append (list (first y)) x)) drvi nviw)]))

; Creates the sprite list from a given sprite sheet.
; Sprite sheets are expected to be 512 by 512.
; Sprite list contains 64 tiles to be indexed from.
; image -> ListOf[image]
(define
  (make-sprite-list SHEET)
  (flatten
   (for/list ([i (range 8)])
     (for/list ([j (range 8)])
       (freeze
        (* j 64)
        (* i 64)
        64
        64
        (bitmap/file
         (string->path SHEET)))))))

; Initiates the start of a level.
; integer -> game
(define
  (start-level LEVEL)
  (let*
      ([ledm (read-level-data (list-ref LEVELS LEVEL))]
       [leda (if (= 2 LEVEL) (place-box ledm (make-list 7 (list 21 6))) ledm)]
       [spli (make-sprite-list (list-ref SHEETS LEVEL))])
    (make-game
     (list-ref STARTS LEVEL)
     720 0
     (list*
      (list-ref TITLES LEVEL)
      LEVEL
      (append
       (make-list (list-ref THINGS LEVEL) #f)
       (if (= 2 LEVEL) (list (make-list 7 (list 21 6))) empty)))
     leda
     spli
     (make-draw-screen (list-ref STARTS LEVEL) 0 leda spli)
     #f 0 0)))


; ###  ###   ##  # # #
; #  # #  # #  # # # #
; #  # #  # #### # # #
; #  # ###  #  # # # #
; #  # #  # #  #  # #
; ###  #  # #  #  # #


; Draws clock hands representing health.
; integer -> image
(define
  (health-clock-hands HEALTH)
  (let*
      ([HOUR   (/ HEALTH 60)]
       [MINUTE (- HEALTH (* 60 (floor HOUR)))])
    (overlay
     (rotate
      (* HOUR -30)
      (above
       (rectangle 2 24 'solid 'black)
       (rectangle 2 16 0 'red)))
     (rotate
      (* MINUTE -6)
      (above
       (rectangle 1 21 'solid 'red)
       (rectangle 1 19 0 'red)))
     (square 64 0 'red))))

; Draw View Window
; Draws view screen from draw-view.
; ListOf[ListOf[integer]] -> image
(define
  (draw-view-window DRAW)
  (freeze
   (apply
    above
    (map
     (lambda (lst)
       (apply beside lst))
     DRAW))))

; Animation x/y
; Output transformed coordinate based off of time.
; Expected time input is [0 2].
; real real real -> real
(define
  (anim-x x y t)
  (+
   (* x               (cos (* pi t 0.25)))
   (* y (abs (- t 1)) (sin (* pi t 0.25)))))
(define
  (anim-y x y t)
  (+
   (* x                  (sin (* pi t 0.25)))
   (* y -1 (abs (- t 1)) (cos (* pi t 0.25)))))

; Tile Flip
; Flips the list structure for proper graphics.
; ListOf[ListOf[integer]] -> ListOf[ListOf[integer]]
(define
  (inv tile)
  (build-list
   9
   (lambda (ind)
     (map
      (lambda (lst) (list-ref lst (- 8 ind)))
      tile))))

; Time Rotation Animation
; Takes in draw-view and buffer-view, and animates it with time t.
; ListOf[ListOf[integer]] ListOf[ListOf[integer]] real -> image
(define
  (anim-time tile1 tile2 t)
  (freeze
   64 64 (* 64 9) (* 64 9)
   (let ([tile (if (<= t 1) tile1 (inv tile2))]
         [mask (list -4 4 -3 3 -2 2 -1 1 0)])
     (foldl
      (lambda (lst y pic1)
        (foldl
         (lambda (til x pic2)
           (overlay/offset
            til
            (* -64 (anim-x x y t))
            (* -64 (anim-y x y t))
            pic2))
         pic1
         lst
         (range -4 5)))
      (square (* 64 11) 'solid 'black)
      (map (lambda (x) (list-ref tile (+ x 4))) mask)
      (map - mask)))))

; Displays the game sreen.
; Shows animations if any.
; game -> image
(define
  (game-view GAME)
  (let ([gnty (game-anim-type GAME)]
        [gnti (game-anim-timer GAME)]
        [drvi (draw-view-window (game-draw-view GAME))]
        [mvsp (/ MOVESPEED 64)]
        [spri (game-sprite-list GAME)])
    (freeze
     (overlay
      (above
       (beside
        (overlay
         ;(text (string-join (map number->string (game-position GAME)) " ") 10 'white)
         (if
          (> (game-health GAME) 0)
          (list-ref spri 63)
          (list-ref spri 15))
         (if
          (> gnti 0)
          (cond
            [(= 1 gnty)
             (anim-time
              (game-buffer-view GAME)
              (game-draw-view GAME)
              (/ gnti TIMESPEED 1/2))]
            [(= -1 gnty)
             (anim-time
              (game-draw-view GAME)
              (game-buffer-view GAME)
              (/ (- TIMESPEED gnti) TIMESPEED 1/2))]
            [(= 4 gnty) (freeze 0 (- 64 (/ gnti mvsp)) (* 9 64) (* 9 64) drvi)]
            [(= 5 gnty) (freeze (/ gnti mvsp) 0 (* 9 64) (* 9 64) drvi)]
            [(= 2 gnty) (freeze 0 (/ gnti mvsp) (* 9 64) (* 9 64) drvi)]
            [(= 3 gnty) (freeze (- 64 (/ gnti mvsp)) 0 (* 9 64) (* 9 64) drvi)]
            [else drvi])
          (draw-view-window (game-draw-view GAME))))
        (overlay/align
         'center 'top
         (apply
          above
          (append
           (list
            (overlay
             (health-clock-hands (game-health GAME))
             (cond
               [(third (game-meta-data GAME)) (list-ref spri 30)]
               [(< (game-health GAME) 180) (list-ref spri 29)]
               [else (list-ref spri 31)]))
            (overlay
             (list-ref
              spri
              (inexact->exact
               (ceiling
                (- 27
                   (/
                    (* 3
                       (- (third (game-position GAME)) 1))
                    (if
                     (= 3 (second (game-meta-data GAME)))
                     18
                     (+ (second (game-meta-data GAME)) 1)))))))
             (list-ref
              (list
               (square 64 0 'red)
               (rotate 90 (list-ref spri 28))
               (list-ref spri 28))
              (game-direction GAME))))
           (build-list
            5
            (lambda (x)
              (if
               (with-handlers ([exn:fail? (lambda (f) #f)])
                 (list-ref (game-meta-data GAME) (+ x 3)))
               (list-ref spri (- 62 x))
               (square 64 0 'red))))))
         (rectangle 64 (* 9 64) 'solid 'darkgray)))
       (overlay
        (text (first (game-meta-data GAME)) 40 'white)
        (rectangle 640 64 'solid 'darkgray)))
      (square 640 'solid 'black)))))

;  ##  #  # #####  ##   ##  #### #   # ####  ##
; #  # #  #   #   #  # #  # #    ##  # #    #  #
; #    #  #   #    #   #    #### # # # ####  #
; #    #  #   #     #  #    #    # # # #      #
; #  # #  #   #   #  # #  # #    #  ## #    #  #
;  ##   ##    #    ##   ##  #### #   # ####  ##

(define-struct anim (view next timer))

; Draw animation
; anim -> image
(define
  (draw-anim ANIM)
  (let*
      ([time (anim-timer ANIM)]
       [levl (second (game-meta-data (anim-next ANIM)))]
       [indx (max 0 (inexact->exact (floor (/ time 30))))])

    (cond
      [(>= time 170)
       (freeze
        (let ([s (inexact->exact (floor (* (- 200 time) 255/30)))])
          (overlay
           (square 640 'solid (make-color s s s s))
           (game-view (anim-view ANIM)))))]
      [(>= time 150) (square 640 'solid 'white)]
      [(>= time 120)
       (freeze
        (let ([s (inexact->exact (floor (* (- time 120) 255/30)))])
          (overlay
           (square 640 'solid (make-color s s s s))
           (game-view (anim-next ANIM)))))]
      [(>= time 90) (game-view (anim-next ANIM))]
      [else
       (overlay
        (above
         (beside
          (overlay
           (text (list-ref (list-ref DIALOG levl) indx) 12 'black)
           (rectangle 130 40 'solid 'lightgray)
           (rectangle 140 50 'solid 'darkgray))
          (rectangle 64 1 0 'red))
         (rectangle 1 192 0 'red))
        (game-view (anim-next ANIM)))])))

; Updates anim struct.
; anim -> (or anim game)
(define
  (update-anim ANIM)
  (if
   (<= (anim-timer ANIM) 0)
   (anim-next ANIM)
   (make-anim
    (anim-view ANIM)
    (anim-next ANIM)
    (- (anim-timer ANIM) ANIMATIONSPEED))))


(define-struct menu (cover back1 back2 x1 y1 x2 y2 select help))

; Start game from beginning
; -> menu
(define
  (start-beginning)
  (let ([sheet (bitmap/file "assets\\.\\sheet0.png")])
    (make-menu
     (crop   0   0 384 384 sheet)
     (crop   0 384 704 704 sheet)
     (crop 704 384 704 704 sheet)
     50 0 0 0 0 #f)))

; Draw main menu screen
; menu -> image
(define
  (draw-menu MENU)
  (overlay
   (if
    (menu-help MENU)
    (overlay
     (text "Use the WASD or arrow keys to move around.\n
Use Q/E or Page Up/Down to change the time-viewing axis.\n
When viewing a time axis, the red side is the past, while the\ngreen side is the future.\n
The hourglass shows where you are in time.\n
Walk into an object to interact with it.\n
Press enter to advance to the next level or restart when you die.\n
Remember, walking through time consumes your life faster!\n
More information: github.com/Skallos-s/Heartfelt\n
Press enter to return to the main screen." 20 'black)
     ; This rectangle is the background for the help-text.
     (rectangle 600 440 'solid 'lightgray)
     ; This rectange is the border of the background for the help-text.
     (rectangle 610 450 'solid 'darkgray))
    (overlay
     (above
      (rectangle 1 400 0 'red)
      (beside
       (overlay
        (text "START" 40 'black)
        (rectangle 140 45 'solid
                   (if (menu-select MENU) 'lightgray 'darkgray))
        (rectangle 150 55 'solid 'dimgray))
       (rectangle 50 1 0 'red)
       (overlay
        (text "HELP" 40 'black)
        (rectangle 140 45 'solid
                   (if (menu-select MENU) 'darkgray 'lightgray))
        (rectangle 150 55 'solid 'dimgray))))
     (menu-cover MENU)))
   (place-image/align
    (menu-back1 MENU)
    (- (menu-x1 MENU))
    (- (menu-y1 MENU))
    'left 'top
    (place-image/align
     (menu-back2 MENU)
     (- (menu-x2 MENU))
     (- (menu-y2 MENU))
     'left 'top
     (square 640 'solid (make-color 48 48 48))))))

; Update menu background animation
; menu -> menu
(define
  (update-menu MENU)
  (make-menu
   (menu-cover MENU)
   (menu-back1 MENU)
   (menu-back2 MENU)
   (rremainder (+ 0.1 (menu-x1 MENU)) 64)
   (rremainder (+ 0.1 (menu-y1 MENU)) 64)
   (rremainder (+ 0.1 (menu-x2 MENU)) 64)
   (rremainder (+ 0.6 (menu-y2 MENU)) 64)
   (menu-select MENU)
   (menu-help MENU)))

; Menu key handler
; Handles key presses on the main menu.
;menu string -> (or menu anim)
(define
  (menu-key MENU key)
  (cond
    [(and
      (not (menu-help MENU))
      (or
       (string=? key "w")
       (string=? key "a")
       (string=? key "s")
       (string=? key "d")
       (string=? key "left")
       (string=? key "right")
       (string=? key "up")
       (string=? key "down")))
     (struct-copy menu MENU [select (not (menu-select MENU))])]
    [(and (string=? key "\r") (not (menu-select MENU)))
     (struct-copy menu MENU [help (not (menu-help MENU))])]
    [(and (string=? key "\r") (menu-select MENU))
     (make-anim2
      (read-level-data "data\\.\\scene.dat")
      (make-sprite-list "assets\\.\\sheet4.png")
      -15 #t)]
    [else MENU]))
      

(define-struct anim2 (scene sprite timer intro))

; Update intro/outro animation
; anim2 -> (or anim2 game menu)
(define
  (update-anim2 ANIM)
  (cond
    [(and (anim2-intro ANIM) (>= (anim2-timer ANIM) 510))
     (make-anim 0 (start-level 0) 169)]
    [(and (not (anim2-intro ANIM)) (>= (anim2-timer ANIM) 255))
     (start-beginning)]
    [else
     (struct-copy anim2 ANIM [timer (+ ANIMATIONSPEED (anim2-timer ANIM))])]))

; Processes skippin intro/outro animations
; anim2 string -> (or anim2 game menu)
(define
  (key-anim2 ANIM key)
  (cond
    [(and (string=? key "\r") (anim2-intro ANIM))
     (make-anim 0 (start-level 0) 0)]
    [(and (string=? key "\r") (not (anim2-intro ANIM)))
     (start-beginning)]
    [else ANIM]))

; Create View Window
; ListOf[ListOf[integer]] ListOf[image] -> image
(define
  (create-view-window DATA SPRI)
  (freeze
   (overlay/align
    'left 'top
    (apply
     above
     (map
      (lambda (x)
        (apply
         beside
         (map
          (lambda (y)
            (list-ref SPRI y))
          x)))
      DATA))
    (square 640 'solid 'darkgray))))

; Draw intro/outro animation
; anim2 -> image
(define
  (draw-anim2 ANIM)
  (freeze
   (let*
       ([time (anim2-timer ANIM)]
        [spri (anim2-sprite ANIM)]
        [scen (anim2-scene ANIM)]
        [modl (remainder (inexact->exact (floor time)) 16)]
        [anim (* 8 (+ 2 (inexact->exact (floor (/ modl 4)))))])
     (if
      (anim2-intro ANIM)
      (cond
        [(<= time 0) (square 640 'solid 'white)] ; 0
        [(<= time 30) ; 1
         (let ([s (inexact->exact (floor (* (- 30 time) 255/30)))])
           (overlay
            (square 640 'solid (make-color s s s s))
            (place-image
             (list-ref spri 22)
             544 416
             (create-view-window (first scen) spri))))]
        [(<= time 45) ; 2
         (place-image
          (list-ref spri 22)
          544 416
          (create-view-window (first scen) spri))]
        [(<= time 75) ; 3A
         (place-image
          (list-ref spri 22)
          544 416
          (place-image
           (overlay
            (text "I'm leaving for work, Aria." 12 'black)
            (rectangle 140 15 'solid 'lightgray)
            (rectangle 150 25 'solid 'darkgray))
           444 416
           (create-view-window (first scen) spri)))]
        [(<= time 105) ; 3B
         (place-image
          (list-ref spri 22)
          544 416
          (place-image
           (overlay
            (text "Today we are experimenting\non that time orb we discovered!" 12 'black)
            (rectangle 175 30 'solid 'lightgray)
            (rectangle 185 40 'solid 'darkgray))
           430 416
           (create-view-window (first scen) spri)))]
        [(<= time 120) ; 4
         (place-image
          (list-ref spri 22)
          544 416
          (create-view-window (first scen) spri))]
        [(<= time 150) ; 5
         (place-image
          (list-ref spri 22)
          544 416
          (place-image
           (overlay
            (text "Mmphh... *snooze*" 12 'black)
            (rectangle 110 15 'solid 'lightgray)
            (rectangle 120 25 'solid 'darkgray))
           244 236
           (create-view-window (first scen) spri)))]
        [(<= time 180) ; 6
         (let ([s (inexact->exact (floor (* (- time 150) 255/30)))])
           (overlay
            (square 640 'solid (make-color s s s s))
            (place-image
             (list-ref spri 22)
             544 416
             (create-view-window (first scen) spri))))]
        [(<= time 195) (square 640 'solid 'white)] ; 7
        [(<= time 225) ; 8
         (let ([s (inexact->exact (floor (* (- 225 time) 255/30)))])
           (overlay
            (square 640 'solid (make-color s s s s))
            (place-image
             (list-ref spri 22)
             288 416
             (place-image
              (list-ref spri anim)
              288 224
              (create-view-window (second scen) spri)))))]
        [(<= time 240) ; 9
         (place-image
          (list-ref spri 22)
          288 416
          (place-image
           (list-ref spri anim)
           288 224
           (create-view-window (second scen) spri)))]
        [(<= time 270) ; 10
         (place-image
          (list-ref spri 22)
          288 416
          (place-image
           (overlay
            (text "Oh dear..." 12 'black)
            (rectangle 60 15 'solid 'lightgray)
            (rectangle 70 25 'solid 'darkgray))
           340 416
           (place-image
            (list-ref spri anim)
            288 224
            (create-view-window (second scen) spri))))]
        [(<= time 300) (square 640 'solid 'black)] ; 11
        [(<= time 315) (create-view-window (first scen) spri)] ; 12A
        [(<= time 320) (square 640 'solid 'black)] ; 12B
        [(<= time 325) (create-view-window (first scen) spri)] ; 12C
        [(<= time 330) (square 640 'solid 'black)] ; 12D
        [(<= time 345) (create-view-window (first scen) spri)] ; 12E
        [(<= time 375) (square 640 'solid 'black)] ; 13
        [(<= time 465) ; 14
         (place-image
          (list-ref spri 14)
          288 288
          (place-image
           (list-ref spri anim)
           288 224
           (create-view-window (third scen) spri)))]
        [(<= time 480) ; 15
         (place-image
          (list-ref spri anim)
          288 (+ 224 (* (- time 465) 64/15))
          (place-image
           (list-ref spri 14)
           288 288
           (create-view-window (third scen) spri)))]
        [else ; 16
         (let ([s (inexact->exact (floor (* (- time 480) 255/30)))])
           (overlay
            (square 640 'solid (make-color s s s s))
            (place-image
             (list-ref spri anim)
             288 288
             (place-image
              (list-ref spri 14)
              288 288
              (create-view-window (third scen) spri)))))])
      (cond
        [(<= time 15) ; 1
         (place-image
          (list-ref spri 22)
          288 416
          (place-image
           (list-ref spri 6)
           288 288
           (create-view-window (second scen) spri)))]
        [(<= time 30) ; 2
         (place-image
          (list-ref spri 22)
          288 416
          (place-image
           (list-ref spri 14)
           288 288
           (create-view-window (second scen) spri)))]
        [(<= time 90) ; 3
         (place-image
          (list-ref spri 22)
          288 416
          (place-image
           (overlay
            (text "Aria!" 12 'black)
            (rectangle 30 15 'solid 'lightgray)
            (rectangle 40 25 'solid 'darkgray))
           340 416
           (place-image
            (list-ref spri 14)
            288 288
            (create-view-window (second scen) spri))))]
        [(<= time 120) ; 4
         (let ([s (inexact->exact (floor (* (- time 90) 255/30)))])
           (overlay
            (square 640 'solid (make-color s s s s))
            (place-image
          (list-ref spri 22)
          288 416
          (place-image
           (overlay
            (text "Aria!" 12 'black)
            (rectangle 30 15 'solid 'lightgray)
            (rectangle 40 25 'solid 'darkgray))
           340 416
           (place-image
            (list-ref spri 14)
            288 288
            (create-view-window (second scen) spri))))))]
        [(<= time 135) (square 640 'solid 'white)] ; 5
        [(<= time 165) ; 6
         (let ([s (inexact->exact (floor (* (- 165 time) 255/30)))])
           (overlay
            (text "A Game Made By Skallos" 40 (make-color s s s))
            (square 640 'solid 'white)))]
        [(<= time 195)
         (overlay
          (text "A Game Made By Skallos" 40 'black)
          (square 640 'solid 'white))] ; 7
        [(<= time 225) ; 8
         (let ([s (inexact->exact (floor (* (- time 195) 255/30)))])
           (overlay
            (text "A Game Made By Skallos" 40 (make-color s s s))
            (square 640 'solid 'white)))]
        [else (square 640 'solid 'white)]))))) ; 9

; # # #  ##  ###  #    ###
; # # # #  # #  # #    #  #
; # # # #  # #  # #    #  #
; # # # #  # ###  #    #  #
;  # #  #  # #  # #    #  #
;  # #   ##  #  # #### ###

; Push Current and future boxes if possible
(define
  (push-box BOXS DATA POSN DELT)
  (if
   (and
    (<= 1 (third POSN) 7)
    (equal?
     (list (first POSN) (second POSN))
     (list-ref BOXS (sub1 (third POSN))))
    (let*
        ([ind (sub1 (third POSN))]
         [npo (map + (list-ref BOXS ind) (list (first DELT) (second DELT)))]
         [til (list-ref (list-ref (list-ref DATA (add1 ind)) (second npo)) (first npo))]
         [wal (list-ref (list-ref (list-ref DATA ind) (second POSN)) (first POSN))])
      (cond
        [(= til 44) #t]
        [(= til 49) #t]
        [(= til 01) #t]
        [(= til 09) #t]
        [(= til 11) #t]
        [(= til 12) #t]
        [else #f])))
   (map
    (lambda (box mov ind)
      (let*
          ([npo (map + box (list (first DELT) (second DELT)))]
           [til (list-ref (list-ref (list-ref DATA ind) (second npo)) (first npo))]
           [wal (list-ref (list-ref (list-ref DATA ind) (second POSN)) (first POSN))])
        (cond
          [mov box]
          [(and (>= wal 32) (not (= wal 48))) box]
          [(= til 44) npo]
          [(= til 49) npo]
          [(= til 01) npo]
          [(= til 09) npo]
          [(= til 11) npo]
          [(= til 12) npo]
          [else box])))
    BOXS
    (build-list 7 (lambda (x) (>= x (third POSN))))
    (range 1 8))
   BOXS))
  
; Clears level-data of time push boxes and places both new time boxes, and shadows.
; ListOf[ListOf[ListOf[integer]]] ListOf[ListOf[integer]] -> ListOf[ListOf[ListOf[integer]]]
(define
  (place-box DATA BOXS)
  (map
   (lambda (tim box)
     (if
      (not box) tim
      (list-update
       tim (second box)
       (lambda (col)
         (list-update
          col (first box)
          (lambda (row)
            (cond
              [(= row 01) 48]
              [(= row 11) 48]
              [(= row 44) 50]
              [(= row 49) 50]
              [(= row 09) 51]
              [(= row 12) 51]
              [else row])))))))
   (place-box-shadow (remove-box DATA) (first BOXS))
   (append (list #f) BOXS (list #f))))

; Places box shadows onto level-dat.
; ListOf[ListOf[ListOf[integer]]] ListOf[integer] -> ListOf[ListOf[ListOf[integer]]]
(define
  (place-box-shadow DATA BOXP)
  (map
   (lambda (tim)
     (list-update
      tim (second BOXP)
      (lambda (col)
        (list-update
         col (first BOXP)
         (lambda (row)
           (cond
             [(= row 01) 11]
             [(= row 44) 49]
             [(= row 09) 12]
             [else row]))))))
   DATA))

; Clears level-data of time push boxes.
; ListOf[ListOf[ListOf[integer]]] -> ListOf[ListOf[ListOf[integer]]]
(define
  (remove-box DATA)
  (map
   (lambda (tim)
     (map
      (lambda (col)
        (map
         (lambda (row)
           (cond
             [(= row 48) 01]
             [(= row 11) 01]
             [(= row 12) 09]
             [(= row 51) 09]
             [(= row 50) 44]
             [(= row 49) 44]
             [else row]))
         col))
      tim))
   DATA))

; Updates meta-data when collecting items or triggering events.
; list -> list
(define
  (collect-item META POSN)
  (let ([LEVEL (second META)]
        [X (first POSN)]
        [Y (second POSN)]
        [Z (third POSN)])
    (cond
      [(= 0 LEVEL)
       (cond
         [(and (= 23 X) (= 15 Y)) (list-set META 2 #t)]
         [(and (= 11 X) (=  2 Y)) (list-set META 3 #t)]
         [(and (= 12 X) (= 30 Y)) (list-set META 4 #t)]
         [else META])]
      [(= 1 LEVEL)
       (cond
         [(and (=  1 X) (=  1 Y))         (list-set META 2 #t)]
         [(and (=  3 X) (= 15 Y) (= 1 Z)) (list-set META 3 #t)]
         [(and (= 15 X) (=  3 Y))         (list-set META 4 #t)]
         [(and (= 15 X) (=  9 Y) (< 3 Z)) (list-set META 5 #t)]
         [else META])]
      [(= 2 LEVEL)
       (cond
         [(and (= 25 X) (=  8 Y))         (list-set META 3 #t)]
         [(and (= 22 X) (=  2 Y))         (list-set META 4 #t)]
         [(and (= 31 X) (=  3 Y))         (list-set META 5 #t)]
         [(and (= 14 X) (=  2 Y) (= 7 Z)) (list-set META 6 #t)]
         [(and (= 14 X) (=  9 Y))         (list-set META 7 #t)]
         [(index-of (map (lambda (x) (equal? (list 21 2) x)) (last META)) #t)
          (list-set META 8 #t)]
         [else META])]
      [(and (= 3 LEVEL) (= 4 X) (= 4 Y) (= 42 Z)) (list-set META 2 #t)]
      [else META])))
  

; Check for event by position.
; Run event when conditions are met.
; ListOf[ListOf[ListOf[integer]]] integer (or ListOf[integer] bool) bool
;     -> ListOf[ListOf[ListOf[integer]]]
(define
  (check-event DATA META TPSN NPSN)
  (let ([LEVEL (second META)]
        [X (first  TPSN)]
        [Y (second TPSN)]
        [A (first  NPSN)]
        [B (second NPSN)]
        [C (third NPSN)])
    (cond
      [(= 0 LEVEL)
       (cond
         [(and               (=  11 A)    (=   2 B))    (update-level-data DATA 0 NPSN)]
         [(and (fourth META) (<= 22 X 23) (=  24 Y))    (update-level-data DATA 1 TPSN)]
         [(and               (=  19 X)    (=  27 Y))    (update-level-data DATA 2 TPSN)]
         [(and               (=  12 A)    (=  30 B))    (update-level-data DATA 3 NPSN)]
         [(and (fifth META)  (=  21 X)    (<= 14 Y 15)) (update-level-data DATA 4 TPSN)]
         [(and               (=  23 A)    (=  15 B))    (update-level-data DATA 5 TPSN)]
         [else DATA])]
      [(= 1 LEVEL)
       (cond
         [(and (= 1 C)       (=  3 A) (= 15 B)) (update-level-data DATA  6 NPSN)]
         [(and (fourth META) (= 12 X) (=  3 Y)) (update-level-data DATA  7 TPSN)]
         [(and               (= 15 A) (=  3 B)) (update-level-data DATA  8 NPSN)]
         [(and (fifth META)  (= 12 X) (= 15 Y) (<= 1 C 2)) (update-level-data DATA  9 TPSN)]
         [(and               (= 15 A) (=  9 B)) (update-level-data DATA 10 NPSN)]
         [(and (sixth META)  (=  7 X) (=  1 Y)) (update-level-data DATA 11 NPSN)]
         [(and               (=  1 X) (=  1 Y)) (update-level-data DATA 12 NPSN)]
         [else DATA])]
      [(= 2 LEVEL)
       (cond
         [(and              (= 25 A) (= 8 B))    (update-level-data DATA  13 NPSN)]
         [(and              (= 22 A) (= 2 B))    (update-level-data DATA  14 NPSN)]
         [(and              (= 31 A) (= 3 B))    (update-level-data DATA  15 NPSN)]
         [(and (= 7 C)      (= 14 A) (= 2 B))    (update-level-data DATA  16 NPSN)]
         [(and              (= 14 A) (= 9 B))    (update-level-data DATA  17 NPSN)]
         [(and
           (not (ninth META))
           (index-of (map (lambda (x) (equal? (list 21 2) x)) (last META)) #t))
          (update-level-data DATA 18 NPSN)]
         [(and (= 14 X) (= 7 Y)) (update-level-data
                                  DATA  19
                                  (list
                                   (fourth META)
                                   (fifth META)
                                   (sixth META)
                                   (third NPSN)))]
         [(and (eighth META) (= 7 C) (= 14 X) (= 4 Y)) (update-level-data DATA  20 TPSN)]
         [(and (seventh META) (=  8 X) (<= 5 Y 6))     (update-level-data DATA  21 TPSN)]
         [(and (= 20 X) (= 2 Y)) (update-level-data DATA 22 TPSN)]
         [else DATA])]
      [(and (= 3 LEVEL) (=  4 A) (= 4 B)) (update-level-data DATA 23 NPSN)]
      [else DATA])))

; Update level-data according to game events.
; Additional argument needed for some events.
; ListOf[ListOf[ListOf[integer]]] integer ListOf[integer]
;     -> ListOf[ListOf[ListOf[integer]]]
;  0 L1 Pick up key
;  1 L1 Open door (POSN)
;  2 L1 Detonate
;  3 L1 Pick up pick
;  4 L1 Break wall (POSN)
;  5 L1 Consume time

;  6 L2 Pick up green key
;  7 L2 Open green door (POSN)
;  8 L2 Pick up red key
;  9 L2 Open red door (POSN)
; 10 L2 Pick of sledgehammer
; 11 L2 Break hydrant
; 12 L2 Consume time

; 13 L3 Pick up red key
; 14 L3 Pick up green key
; 15 L3 Pick up blue key
; 16 L3 Pick up yellow key
; 17 L3 Pick up key
; 18 L3 Finish puzzle %%%%%%%%%%%%%%%%%%
; 19 L3 Open multicoor door (POSN) (RGB Key Booleans + (third POSN))
; 20 L3 Open door
; 21 L3 Open yellow door (POSN)
; 22 L3 Consume time
; 23 L4 Reach ending
(define
  (update-level-data DATA EVENT POSN)
  (cond
    [(= 0 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 2)
          (list
           (map
            (lambda (col) (cond [(= col 41) 37] [(= col 05) 01] [else col]))
            (list-ref time 2)))
          (drop time 3))))
      DATA (list #t #f #f #f #t))]
    [(= 1 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 24)
          (list
           (append
            (take (list-ref time 24) 22) (list 01 01) (drop (list-ref time 24) 24)))
          (drop time 25))))
      DATA (list #t #f (< (third POSN) 2) (< (third POSN) 3) #t))]
    [(= 2 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 26)
          (let ([row (list-ref time 26)])
            (list
             (append (take row 9) (list 37 37 03 03) (drop row 13))))
          (let ([row (list-ref time 27)])
            (list (append (take row 9) (list 37 37 03 03 38 38 38 38 44 01 43) (drop row 20))))
          (let ([row (list-ref time 28)])
            (list (append (take row 9) (list 37 37 03 03) (drop row 13))))
          (drop time 29))))
      DATA (list #t #f (< (third POSN) 2) (< (third POSN) 3) #t))]
    [(= 3 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append (take time 30) (list (list-set (list-ref time 30) 12 01)) (drop time 31))))
      DATA (list #t #f #f #f #t))]
    [(= 4 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (let ([r14 (list-ref time 14)]
               [r15 (list-ref time 15)])
           (append
            (take time 14)
            (list (list-set r14 21 (if (= 14 (second POSN)) 01 (list-ref r14 21))))
            (list (list-set r15 21 (if (= 15 (second POSN)) 01 (list-ref r15 21))))
            (drop time 16)))))
      DATA (list #t #f (< (third POSN) 2) (< (third POSN) 3) #t))]
    [(= 5 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 14) (list (list-set (list-ref time 14) 23 46)) (drop time 15))))
      DATA (list #t #f #f #f #t))]
    [(= 6 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 15)
          (list (append (take (list-ref time 15) 3) (list 05) (drop (list-ref time 15) 4)))
          (drop time 16))))
      DATA (list #t #f #t #t #t #t #t))]
    [(= 7 EVENT)
     (map
      (lambda (time skip)
        (if
         (= skip 0) time
         (append
          (take time 3)
          (list (map (lambda (col) (if (= col 41) skip col)) (list-ref time 3)))
          (drop time 4))))
      DATA (list 0 38 38 38 38 05 0))]
    [(= 8 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 3)
          (list
           (map
            (lambda (col) (cond [(= col 48) 38] [(= col 10) 05] [else col]))
            (list-ref time 3)))
          (drop time 4))))
      DATA (list 0 #f #f #f #f #f #t))]
    [(= 9 EVENT)
     (map
      (lambda (time skip)
        (if
         (= skip 0) time
         (append
          (take time 15)
          (list (map (lambda (col) (if (= col 40) skip col)) (list-ref time 15)))
          (drop time 16))))
      DATA (list 0 38 05 43 43 43 0))]
    [(= 10 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 9)
          (list (map (lambda (col) (if (= col 11) 02 col)) (list-ref time 9)))
          (drop time 10))))
      DATA (list #t #f #f #f #f #f #t))]
    [(= 11 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (list (first time))
          (list (list 34 07 44 51 51 51 51 50 51 51 51 51 51 51 51 51 51 51 34))
          (map
           (lambda (row)
             (map
              (lambda (col)
                (cond
                  [(= col 35) 01]
                  [(= col 36) 03]
                  [(= col 37) 04]
                  [(= col 38) 05]
                  [(= col 39) 08]
                  [else col]))
              row))
           (drop-right (drop time 2) 1))
          (list (last time)))))
      DATA (list #t #f #f #f #f #f #t))]
    [(= 12 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 1)
          (list
           (map
            (lambda (col) (if (= col 44) 45 col))
            (list-ref time 1)))
          (drop time 2))))
      DATA (list 0 #f #f #f #f #f #t))]
    [(= 13 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 8)
          (list
           (map
            (lambda (col) (cond [(= col 42) 47] [(= col 06) 01] [else col]))
            (list-ref time 8)))
          (drop time 9))))
      DATA (list #t #f #f #f #f #f #f #f #t))]
    [(= 14 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 2)
          (list
           (map
            (lambda (col) (if (= col 05) 01 col))
            (list-ref time 2)))
          (drop time 3))))
      DATA (list #t #f #f #f #f #f #f #f #t))]
    [(= 15 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 3)
          (list
           (map
            (lambda (col) (if (= col 04) 01 col))
            (list-ref time 3)))
          (drop time 4))))
      DATA (list #t #f #f #f #f #f #f #f #t))]
    [(= 16 EVENT)
     (list-update
      (list-update
       DATA 7
       (lambda (x)
         (list-update
          x 2
          (lambda (y) (list-set y 14 02)))))
      6 (lambda (z)
          (list-update
           z 2
           (lambda (w) (list-set w 14 40)))))]
    [(= 17 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 9)
          (list
           (map
            (lambda (col) (if (= col 10) 01 col))
            (list-ref time 9)))
          (drop time 10))))
      DATA (list #t #f #f #f #f #f #f #f #t))]
    [(= 18 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (map
          (lambda (col)
            (map
             (lambda (row)
               (cond
                 [(= row 44) 01]
                 [(= row 45) 05]
                 [(= row 49) 11]
                 [(= row 50) 48]
                 [else row]))
             col))
          time)))
      DATA (list #t #f #f #f #f #f #f #f #t))]
    [(= 19 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 7)
          (list
           (map
            (lambda (col)
              (cond
                [(and (= col 36) (first  POSN)) 37]
                [(and (= col 37) (second POSN)) 38]
                [(and (= col 38) (third  POSN)) 01]
                [else col]))
            (list-ref time 7)))
          (drop time 8))))
      DATA (list #t #f
                 (< (fourth POSN) 2) (< (fourth POSN) 3)
                 (< (fourth POSN) 4) (< (fourth POSN) 5)
                 (< (fourth POSN) 6) (< (fourth POSN) 7) #t))]
    [(= 20 EVENT)
     (list-update
      DATA 7
      (lambda (x)
        (list-update
         x 4
         (lambda (y) (list-set y 14 01)))))]
    [(= 21 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 5)
          (list
           (map (lambda (col) (if (= col 39) 01 col)) (list-ref time 5))
           (map (lambda (col) (if (= col 39) 01 col)) (list-ref time 6)))
          (drop time 7))))
      DATA (list #t #f
                 (< (third POSN) 2) (< (third POSN) 3)
                 (< (third POSN) 4) (< (third POSN) 5)
                 (< (third POSN) 6) (< (third POSN) 6) #t))]
    [(= 22 EVENT)
     (map
      (lambda (time skip)
        (if
         skip time
         (append
          (take time 2)
          (list
           (map
            (lambda (col) (if (= col 52) 53 col))
            (list-ref time 2)))
          (drop time 3))))
      DATA (list #t #f #f #f #f #f #f #f #t))]
    [(= 23 EVENT) (read-level-data "data\\.\\final.dat")]
    [else DATA]))

; Game Key Function
; World key handler for game specifically.
; game string -> game
(define
  (game-key GAME KEYP)
  (if
   (or
    (> (game-anim-timer GAME) 0)
    (third (game-meta-data GAME))
    (<= (game-health GAME) 0))
   (cond
     [(and
       (<= (game-health GAME) 0)
       (string=? KEYP "\r"))
      (start-level (second (game-meta-data GAME)))]
     [(and
       (third (game-meta-data GAME))
       (string=? KEYP "\r")
       (< (second (game-meta-data GAME)) 2))
      (make-anim
       GAME
       (start-level (add1 (second (game-meta-data GAME))))
       200)]
     [else GAME])
   (let*
       ([cpos (game-position GAME)] ; Current Position
        [cdir (game-direction GAME)] ; Current Direction
        [cdat (game-level-data GAME)] ; Current Level Data
        [cspr (game-sprite-list GAME)] ; Current Sprite List
        [cmet (game-meta-data GAME)]; Meta-data
        [arro ; Movement Direction
         (cond
           [(or (string=? KEYP "up"   ) (string=? KEYP "w")) 0]
           [(or (string=? KEYP "right") (string=? KEYP "d")) 1]
           [(or (string=? KEYP "down" ) (string=? KEYP "s")) 2]
           [(or (string=? KEYP "left" ) (string=? KEYP "a")) 3]
           [else -1])]
        [sign (if (<= 1 arro 2) 1 -1)] ; Movement Sign
        [delt ; Movement Delta (Calculated Using cdir)
         (cond
           [(negative? arro) '(0 0 0)]
           [(even? arro)
            (list-ref (list (list 0 sign 0) (list sign 0 0) (list 0 0 sign)) cdir)]
           [else (list-ref (list (list sign 0 0) (list 0 0 sign) (list 0 sign 0)) cdir)])]
        [tpos (map + cpos delt)] ; Proposed Position
        [npos (if ; Actual New Position (Cannot move into walls)
               (<=
                32
                (foldl
                 (lambda (x y) (list-ref y x))
                 cdat (reverse tpos)))
               cpos tpos)]
        [dird ; Direction Delta
         (cond
           [(or (string=? KEYP "prior") (string=? KEYP "q"))  1]
           [(or (string=? KEYP "next" ) (string=? KEYP "e")) -1]
           [else 0])]
        [ndir (modulo (- cdir dird) 3)] ; New Direction
        [mmet (if
               (and
                (equal?
                 (list (first npos) (second npos))
                 (list 4 4))
                (= 2 (second (game-meta-data GAME))))
               (list-set cmet 1 3) cmet)]
        [nbox (cond ; New Box Positions
                [(not (= (second mmet) 2)) #f]
                [(and
                  (<= 21 (first npos) 22)
                  (= 8 (second npos)))
                 (make-list 7 (list 21 6))]
                [else (push-box (last cmet) cdat tpos delt)])]
        [nmet (if nbox (list-set cmet 9 nbox) mmet)] ; New Meta Data
        [mdat (check-event cdat nmet tpos npos)] ; New Level Data
        [ndat (if (= 2 (second mmet)) (place-box mdat nbox) mdat)]
        [nviw (make-draw-screen npos ndir ndat cspr)] ; New Draw View
        [heal (with-handlers
                  ([exn:fail? (lambda (f) #f)])
                (not
                 (=
                  (list-ref (list-ref (list-ref cdat 1) 2) 20)
                  (list-ref (list-ref (list-ref ndat 1) 2) 20))))])
     (make-game
      npos
      (if heal 720 (game-health GAME))
      ndir
      (collect-item nmet npos)
      ndat
      cspr
      (cond
        [(= 1 (abs dird)) (game-draw-view GAME)]
        [(and (>= arro 0) (equal? npos tpos)) (merge (game-draw-view GAME) nviw arro)]
        [else nviw])
      (if
       (and (= 0 dird) (negative? arro))
       (game-buffer-view GAME)
       (make-draw-screen npos ndir ndat cspr))
      (cond
        [(= 1 (abs dird)) TIMESPEED]
        [(and (>= arro 0) (equal? npos tpos)) MOVESPEED]
        [else 0])
      (cond
        [(= 1 (abs dird)) dird]
        [(and (>= arro 0) (equal? npos tpos)) (+ arro 2)]
        [else 0])))))

; Game Update Function
; Updates world animation timer and follows necessary proceses.
; game -> game
(define
  (game-update GAME)
  (let*
      ([gdv (game-draw-view GAME)]
       [gbv (game-buffer-view GAME)]
       [gti (game-anim-timer GAME)]
       [gty (game-anim-type GAME)]
       [dir (game-direction GAME)]
       [met (game-meta-data GAME)]
       [tmp (struct-copy game GAME [meta-data (list-set met 2 #f)])])
    (cond
      [(and
        (third met)
        (= 3 (second met))
        (= 0 dir)
        (= 0 gti))
       (make-anim2
        (read-level-data "data\\.\\scene.dat")
        (make-sprite-list "assets\\.\\sheet4.png")
        0 #f)]
      [(and (third met) (= 1 dir) (= 0 gti))
       (game-key tmp "q")]
      [(and (third met) (= 2 dir) (= 0 gti))
       (game-key tmp "e")]
      [else
       (make-game
        (game-position GAME)
        (max
         (-
          (game-health GAME)
          (cond
            [(third met) 0]
            [(and
              (> gti 0)
              (or
               (and (= dir 1) (= gty 3))
               (and (= dir 1) (= gty 5))
               (and (= dir 2) (= gty 2))
               (and (= dir 2) (= gty 4))))
             0.75]
            [else 0.15]))
         0)
        dir
        met
        (game-level-data GAME)
        (game-sprite-list GAME)
        (if (and (not (zero? gty)) (<= gti 1)) gbv gdv)
        (if (and (not (zero? gty)) (<= gti 0)) #f  gbv)
        (max 0 (sub1 gti))
        (if (<= gti 0) 0 gty))])))


; # # # ####  ##  #   # ####
; # # # #    #  # #   # #
; # # # #### #### #   # ####
; # # # #    #  #  # #  #
;  # #  #    #  #  # #  #
;  # #  #### #  #   #   ####

; Draws animation, game, or main menu.
; any -> image
(define
  (draw-combined X)
  (scale
   (/ (get-display-backing-scale))
   (cond
     [(anim2? X) (draw-anim2 X)]
     [(menu? X) (draw-menu X)]
     [(anim? X) (draw-anim X)]
     [(game? X) (game-view X)]
     [else empty-image])))

; Handles keypresses for animation, game, or main menu.
; any -> any
(define
  (key-combined X key)
  (cond
    [(anim2? X) (key-anim2 X key)]
    [(menu? X) (menu-key X key)]
    [(anim? X) X]
    [(game? X) (game-key X key)]
    [else X]))

; Updates animation or game.
; any -> any
(define
  (update-combined X)
  (cond
    [(anim2? X) (update-anim2 X)]
    [(menu? X) (update-menu X)]
    [(anim? X) (update-anim X)]
    [(game? X) (game-update X)]
    [else X]))


(big-bang
    (start-beginning)
  (to-draw draw-combined)
  (on-key key-combined)
  (on-tick update-combined 1/30)
  (name "Heartfelt")
  (display-mode 'normal))
