#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(define
  test
  (overlay/align
   'left 'bottom
   (overlay
    (ellipse 10 30 'solid 'maroon)
    (square 50 'solid 'green))
   (overlay/align
    'right 'top
    (star 40 'solid 'blue)
    (overlay/align
     'right 'bottom
     (overlay
      (circle 20 'solid 'white)
      (square 60 'solid 'gray)
      (circle 50 'solid 'red))
     (overlay/align
      'left 'top
      (overlay
       (triangle 50 'solid 'yellow)
       (triangle 100 'solid 'black)
       (rotate 180 (triangle 100 'solid 'purple)))
      (square 150 'solid 'white))))))

(define
  grid
  (for/list
      ([i (range 5)])
    (for/list
        ([j (range 5)])
      (crop (* j 30) (* i 30) 30 30 test))))

(define tiles (flatten grid))


(define
  (req n)
  (list-ref tiles n))

(define hg (map (lambda (x) (make-list 5 (req x))) (range 10 15)))
(define vg (make-list 5 (map req (range 2 25 5))))

test
(apply beside (make-list 5 (apply above  (map req (range 10 15)))))
(apply above  (make-list 5 (apply beside (map req (range 2 25 5)))))


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

(define
  (inv tile)
  (build-list
   5
   (lambda (ind)
     (map
      (lambda (lst) (list-ref lst (- 4 ind)))
      tile))))

(define
  (anim-time tile1 tile2 t)
  (crop
   25 25 150 150
   (let ([tile (if (<= t 1) tile1 (inv tile2))]
         [mask (list -2 2 -1 1 0)])
     (foldl
      (lambda (lst y pic1)
        (foldl
         (lambda (til x pic2)
           (overlay/offset
            til
            (* -30 (anim-x x y t))
            (* -30 (anim-y x y t))
            pic2))
         pic1
         lst
         (list -2 -1 0 1 2)))
      (square 200 'solid 'black)
      (map (lambda (x) (list-ref tile (+ x 2))) mask) (map - mask)))))
      ;(append (take tile 2) (drop tile 3) (list (third tile)))
      ;(list 2 1 -1 -2 0)))))

(anim-time grid hg 1)
(anim-time grid hg 1.4)

(define (draw t)
  (cond
    [(< t 2) (anim-time grid hg t)]
    [(< t 4) (anim-time hg vg (- t 2))]
    [else (anim-time vg grid (- t 4))]))
(define (incr t) (if (< t 6) (+ t 0.01) 0))
(define (stop t) (>= t 6))

(big-bang 0 (on-tick incr) (to-draw draw) );(stop-when stop) );(record? #t))

; Trim List of List by direction.
; ListOf[ListOf[image]] integer -> ListOf[ListOf[image]]
(define
  (trim dat dir)
  (cond
    [(= 2 dir) (drop-right dat 1)]
    [(= 3 dir) (map (lambda (x) (drop-right x 1)) dat)]
    [(= 4 dir) (drop dat 1)]
    [else (map (lambda (x) (drop x 1)) dat)]))

; Places time boxes onto draw-view
; ListOf[ListOf[image]] ListOf[image] ListOf[ListOf[integer]] ListOf[integer] -> ListOf[ListOf[image]]
(define (place-box VIEW SPRI BOXP POSN)
  (let*
      ([Xp (first  POSN)]
       [Yp (second POSN)]
       [Zp (third  POSN)]
       [Xb (- (first  (list-ref BOXP Zp)) Xp -5)]
       [Yb (- (second (list-ref BOXP Zp)) Yp -5)]
       [Xs (- (first  (first BOXP)) Xp -5)]
       [Ys (- (second (first BOXP)) Yp -5)]
       [Pl (list-update
            VIEW Yb
            (lambda (x)
              (list-update
               x Xb
               (lambda (y)
                 (above (list-ref SPRI 19) y)))))])
    (if (or (equal? (list-ref BOXP Zp) (first BOXP)) (= Zp 1))
        Pl
        (list-update
         Pl Ys
         (lambda (x)
           (list-update
            x Xs
            (lambda (y)
              (overlay (list-ref SPRI 19) y))))))))
;[nviw (if (= 2 (second meta)) (place-box mviw cspr (last meta) npos) mviw)])
#|
(if
          (= (second (game-meta-data GAME)) 2)
          (let*
              ([posn (game-position GAME)]
               [box-present
                (list-ref
                 (last (game-meta-data GAME))
                 (+ 1 (third posn)))]
               [box-future (last (last (game-meta-data GAME)))]
               [deltax (- 4 (first  posn))]
               [deltay (- 4 (second posn))])
            (place-image
             (list-ref spri 20)
             (+ 32 (* 64 (+ deltax (first  box-present))))
             (+ 32 (* 64 (+ deltay (second box-present))))
             (place-image
              (if
               (or
                (= (third posn) 7)
                (equal? box-present box-future))
               empty-image
               (list-ref spri 19))
              (+ 32 (* 64 (+ deltax (first  box-future))))
              (+ 32 (* 64 (+ deltay (second box-future))))
              (square (* 64 9) 0 'red))))
          empty)
|#