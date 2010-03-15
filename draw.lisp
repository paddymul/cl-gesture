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



(defmacro with-point (&rest body)
  `(let ((p (make-instance 'point :x x :y y)))
     ,@body))


(defmacro EVENT-WHILE-NIL (display force-output-p &rest event-forms)
  `(unwind-protect 
        (loop while (not 
                     (EVENT-CASE (,display :force-output-p ,force-output-p)
                       ,@event-forms
                       (otherwise ()
                                  ;;Ignore and discard any other event
                                  nil))))))



(defun draw-ev (display prompt-gc draw-title draw-ack)
  (let ((last-point nil)
        (gesture nil)
        (gesture-option-list nil))
    (flet ((dr-lp (window p1 p2)
             (draw-line  window prompt-gc 
                         (point-x p1) (point-y p1)
                         (point-x p2) (point-y p2)))
             (kill-gesture-posibilities 
              ()
               (unwind-protect
                    (mapcar #'(lambda (kill-func)  (funcall kill-func)) gesture-option-list))))
      (EVENT-WHILE-NIL display t
                       (:exposure (count)
                                  ;; Display prompt
                                  (when (zerop count)
                                    (funcall draw-title ))
                                  nil)
                       (:motion-notify  (window x y code)
                                        (with-point 
                                            (dr-lp  window last-point p)
                                          (setf last-point p)
                                          (if (set-last-point gesture p)
                                              (progn
                                                (kill-gesture-posibilities)
                                                 (setq gesture-option-list 
                                                    (display-gesture-posibilities gesture))))
                                          ) nil)
                       (:button-release (x y)
                                        (format t "you have selected ~A" (find-applicable-gestures gesture))
                                        gesture )
                       (:button-press (x y)
                                      (with-point 
                                          (setf last-point p)
                                        (setf gesture (make-gesture p))
                                        (kill-gesture-posibilities)
                                        (setq gesture-option-list 
                                              (display-gesture-posibilities gesture))
                                        )nil))
      (kill-gesture-posibilities)
      )))

(defmethod display-gesture-posibilities ((g gesture))
  (remove-if-not #'functionp (mapcar #'(lambda (side-posi-pair)
              (aif (cadr side-posi-pair)
                   (show-option-list-side  it (car side-posi-pair))))
          (gesture-posibilities g))))
