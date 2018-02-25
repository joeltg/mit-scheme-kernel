(define *canvas-size* 300)
(define *frame-height* 400)
(define *frame-width* 400)
(define *foreground-color* "white")
(define *background-color* "black")
(define *can-use-colors* #t)
(define *frame-x-position* (if (eq? 'unix microcode-id/operating-system) -10 532))
(define *frame-y-position* 0)
(define *frame-width* (if (eq? 'unix microcode-id/operating-system) 400 100))
(define *frame-height* (if (eq? 'unix microcode-id/operating-system) 400 100))

(define (get-pointer-coordinates-default-continuation x y button) *silence*)
(define get-pointer-coordinates-continuation get-pointer-coordinates-default-continuation)

(define-structure
  (canvas (constructor make-canvas (#!optional xmin xmax ymin ymax)))
  (id 0)
  (xmin 0)
  (xmax *canvas-size*)
  (ymin 0)
  (ymax *canvas-size*)
  (frame-width *canvas-size*)
  (frame-height *canvas-size*)
  (frame-x-position 0)
  (frame-y-position 0))

(define (send-canvas canvas action #!optional value)
  (*send* 2 (symbol->json action) (number->string (canvas-id canvas)) (json value)))

(define (canvas-available? . args) #t)

(define (canvas-coordinate-limits canvas)
  (list (canvas-xmin canvas) (canvas-ymax canvas)
        (canvas-xmax canvas) (canvas-ymin canvas)))

(define (canvas-device-coordinate-limits canvas)
  (list 0 0 (canvas-frame-width canvas) (canvas-frame-height canvas)))

(define (canvas-set-coordinate-limits canvas x-left y-bottom x-right y-top)
  (set-canvas-xmin! canvas x-left)
  (set-canvas-ymin! canvas y-bottom)
  (set-canvas-xmax! canvas x-right)
  (set-canvas-ymax! canvas y-top)
  (send-canvas canvas 'set_coordinate_limits (list x-left y-bottom x-right y-top)))

(define (canvas-drag-cursor canvas x y)
  (send-canvas canvas 'drag_cursor (list x y)))

(define (canvas-move-cursor canvas x y)
  (send-canvas canvas 'move_cursor (list x y)))

(define (canvas-reset-clip-rectangle canvas)
  (send-canvas canvas 'reset_clip_rectangle (canvas-coordinate-limits canvas)))

(define (canvas-set-clip-rectangle canvas x-left y-bottom x-right y-top)
  (send-canvas canvas 'set_clip_rectangle (list x-left y-bottom x-right y-top)))

(define (canvas-set-drawing-mode canvas mode)
  (send-canvas canvas 'set_drawing_mode `((mode ,mode))))

(define (canvas-set-line-style canvas style)
  (send-canvas canvas 'set_line_style `((style ,style))))

(define (canvas-clear canvas)
  (send-canvas canvas 'clear))

(define (canvas-flush canvas)
;  *silence*)
  #!unspecific)

(define (canvas-close canvas)
  (send-canvas canvas 'close))

(define (canvas-draw-rect canvas x y width height)
  (send-canvas canvas 'draw_rect (list x y width height)))

(define (canvas-erase-rect canvas x y width height)
  (send-canvas canvas 'erase_rect (list x y width height)))

(define (canvas-draw-rects canvas rects)
  (send-canvas canvas 'draw_rects rects))

(define (canvas-erase-rects canvas rects)
  (send-canvas canvas 'erase_rects rects))

(define (canvas-draw-point canvas x y)
  (send-canvas canvas 'draw_point (list x y)))

(define (canvas-draw-points canvas points)
  (send-canvas canvas 'draw_points points))

(define (canvas-erase-point canvas x y)
  (send-canvas canvas 'erase_point (list x y)))

(define (canvas-erase-points canvas points)
  (send-canvas canvas 'erase_points points))

(define (canvas-draw-line canvas x-start y-start x-end y-end)
  (send-canvas canvas 'draw_line (list x-start y-start x-end y-end)))

(define (canvas-draw-text canvas x y string)
  (send-canvas canvas 'draw_text (list x y string)))

(define (canvas-set-font canvas font-name)
  (send-canvas canvas 'set_font font-name))

(define (canvas-set-background-color canvas color)
  (send-canvas canvas 'set_background_color color))

(define (canvas-set-foreground-color canvas color)
  (send-canvas canvas 'set_foreground_color color))

(define (canvas-get-pointer-coordinates canvas cont)
  (set! get-pointer-coordinates-continuation
    (lambda (x y button)
      (set! get-pointer-coordinates-continuation get-pointer-coordinates-default-continuation)
      (cont x y button)))
  (send-canvas canvas 'get_pointer_coordinates))