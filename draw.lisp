(require :clx)
(use-package :xlib)

(defun display-centered-text
    (window string gcontext height width)	     
  (multiple-value-bind (w a d l r fa fd) (text-extents gcontext string)
    (declare (ignore a d l r))
    (let ((box-height (+ fa fd)))
      
      ;; Clear previous text
      (CLEAR-AREA window
                  :x 0 :y (- height fa)
                  :width width :height box-height)
      
      ;; Draw new text
      (DRAW-IMAGE-GLYPHS window gcontext (round (- width w) 2) height string))))
(defun pop-up2 (host  &key (title "Pick one:") (font "fixed"))
  (let* ((display   (OPEN-DISPLAY host))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (font      (OPEN-FONT display font))
	 (parent-width 400)
	 (parent-height 400)
	 (parent    (CREATE-WINDOW :parent (SCREEN-ROOT screen)
				   :override-redirect :on
				   :x 2800 :y 300
				   :width parent-width :height parent-height
				   :background bg-color
				   :event-mask (MAKE-EVENT-MASK :button-press
                                                                :button-motion
                                                                :button-release
								:exposure)))
	 (prompt    "Press a button...")	 
	 (prompt-gc (CREATE-GCONTEXT :drawable parent
				     :foreground fg-color
				     :background bg-color
				     :font font))
	 (prompt-y  (FONT-ASCENT font))
	 (ack-y     (- parent-height  (FONT-DESCENT font))))
    ;; Present main window
    (labels  (
              (draw-text (text y)
                (display-centered-text parent text prompt-gc y parent-width))
              (draw-title-height (title-text) (draw-text title-text prompt-y))
              (draw-ack (ack-text) (draw-text ack-text ack-y))
              (draw-title () (draw-title-height prompt)))
      (dotimes (foo 5)
        (format t " foo ~A" foo)
        (map-window parent)
        (draw-ev display  prompt-gc #'draw-title #'draw-ack)
        (unmap-window parent)))))


(defun figure-out-direction (x0 y0 x1 y1)
  (let ((ratio (/ (abs (- x0 x1)) (+ (abs (- y0 y1)) 0.000001))))
    (values 
     (if (> ratio 1)
         (if (> x0 x1)
             :left
             :right)
         (if (> y0 y1)
             :up
             :down))
     ratio)))

(defun draw-ev (display prompt-gc draw-title draw-ack)
  (block outer
  (unwind-protect
       (let ((initial-x nil)
             (initial-y nil)
             (last-x nil)
             (last-y nil)
             (gesture nil)
             (last-point-time (get-internal-real-time)))
         (flet (
                (nil-cords ()
                  (setf last-x nil) (setf last-y nil) 
                  (setf initial-x nil) (setf initial-y nil))
                (dr-l  (window x0 y0 x1 y1)
                  (draw-line  window prompt-gc x0 y0 x1 y1))

                #|                (report-dir ()
                (funcall 
                draw-ack 
                (symbol-name 
                (figure-out-direction initial-x initial-y x y))))|#
                (report-dir ()
                  (funcall 
                   draw-ack 
                   (symbol-name 
                    (figure-out-direction initial-x initial-y x y))))
                (dr-p  (window x0 y0)
                  (draw-point  window prompt-gc x0 y0)))

           (loop
              (EVENT-CASE (display :force-output-p t)
                
                (:exposure (count)
                           ;; Display prompt
                           (when (zerop count)
                             (funcall draw-title ))
                           t)
                (:motion-notify  (window x y code)
                                        ;(dr-p win  x y)
                                        ;(dr-l win initial-x initial-y x y)
                                 (dr-l  window last-x last-y x y)
                                 (setf last-x x) (setf last-y y)
                                 (set-last-point 
                                  gesture 
                                  (make-instance 'point :x x :y y))
                                 nil
                                 )
                (:button-release (x y)
                                 (princ "button-release called")
                                 (funcall draw-title ))
                                 (return-from outer gesture))
                (:button-press (x y)
                               ;; Pop up the menu
                               (nil-cords)
                               (setf initial-x x) (setf initial-y y) 
                               (setf last-x x) (setf last-y y) 
                               (setf gesture 
                                     (make-gesture
                                      (make-instance 'point :x x :y y)))
                               (funcall draw-ack
                                        (format nil "You have selected ~a." "foo"))
                               nil)
                (otherwise ()
                           ;;Ignore and discard any other event
                           t)))
           (CLOSE-DISPLAY display))
         gesture))))

