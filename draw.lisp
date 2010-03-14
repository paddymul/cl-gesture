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
      (dotimes (foo 3)
        (map-window parent)
        (format t " foo ~A gesture ~A " foo 
                (draw-ev display  prompt-gc #'draw-title #'draw-ack))
        (unmap-window parent))
      (CLOSE-DISPLAY display)
      )))


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

(defmacro with-point (&rest body)
  `(let ((p (make-instance 'point :x x :y y)))
     ,@body))

(defun draw-ev (display prompt-gc draw-title draw-ack)
  (unwind-protect
       (let ((last-point nil)
             (gesture nil))
         (flet ((dr-lp (window p1 p2)
                  (draw-line  window prompt-gc 
                              (point-x p1) (point-y p1)
                              (point-x p2) (point-y p2))))
           (loop
              while (not 
                     (EVENT-CASE (display :force-output-p t)
                       (:exposure (count)
                                  ;; Display prompt
                                  (when (zerop count)
                                    (funcall draw-title ))
                                  nil)
                       (:motion-notify  (window x y code)
                                        (with-point 
                                            (dr-lp  window last-point p)
                                          (setf last-point p)
                                          (set-last-point gesture p)
                                          ) nil)
                       (:button-release (x y)
                                        (princ "button-release called")
                                        (format t "~A" gesture)
                                        gesture )
                       (:button-press (x y)
                                      (with-point 
                                          (setf last-point p)
                                        (setf gesture (make-gesture p))
                                        (funcall draw-ack
                                                 (format nil "You have selected ~a." "foo"))
                                        )nil)
                       (otherwise ()
                                  ;;Ignore and discard any other event
                                  nil))))))))

(defmacro EVENT-WHILE-NIL (display force-output)