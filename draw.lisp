(require :clx)
(use-package :xlib)

;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;; These functions demonstrate a simple menu implementation described in            |
;;; Kimbrough, Kerry, "Windows to the Future", Lisp Pointers, Oct-Nov, 1987.         |
;;; See functions JUST-SAY-LISP and POP-UP for demonstrations.                       |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+



(defstruct (gesture)
  "A simple menu of text strings."
  (title "choose an item:")


					;((item-window item-string))
  window
  gcontext
  width
  title-width
  item-width
  item-height
  (geometry-changed-p t))			;nil iff unchanged since displayed



(defun create-gesture (parent-window text-color background-color text-font)
  (make-gesture
    ;; Create menu graphics context
    :gcontext (CREATE-GCONTEXT :drawable   parent-window
			       :foreground text-color
			       :background background-color
			       :font       text-font

                               
)
    ;; Create menu window
    :window   (CREATE-WINDOW
		:parent       parent-window
		:class        :input-output
		:x            0			;temporary value
		:y            0			;temporary value
		:width        16		;temporary value
		:height       16		;temporary value		
		:border-width 2
		:border       text-color
		:background   background-color
		:save-under   :on
		:override-redirect :on		;override window mgr when positioning
		:event-mask   (MAKE-EVENT-MASK :button-press
                                               :button-motion
                                               :leave-window					       
					       :exposure))
    :width 400
))


(defparameter *gesture-item-margin* 4
  "Minimum number of pixels surrounding menu items.")




(defun menu-refresh (menu)
 (let* ((gcontext   (menu-gcontext menu))
        (baseline-y (FONT-ASCENT (GCONTEXT-FONT gcontext))))
   
   ;; Show title centered in "reverse-video"
   (let ((fg (GCONTEXT-BACKGROUND gcontext))
	 (bg (GCONTEXT-FOREGROUND gcontext)))
     (WITH-GCONTEXT (gcontext :foreground fg :background bg)
       (DRAW-IMAGE-GLYPHS
	 (menu-window menu)
	 gcontext
	 (round (- (menu-width menu)
		   (menu-title-width menu)) 2)	;start x
	 baseline-y				;start y
	 (menu-title menu))))
   
   ;; Show each menu item (position is relative to item window)
   (dolist (item (menu-item-alist menu))
     (DRAW-IMAGE-GLYPHS
       (first item) gcontext
       0					;start x
       baseline-y				;start y
       (second item)))))
(defun gesture-refresh (gesture)
 (let* ((gcontext   (gesture-gcontext gesture))
        (baseline-y (FONT-ASCENT (GCONTEXT-FONT gcontext))))
   
   ;; Show title centered in "reverse-video"
   (let ((fg (GCONTEXT-BACKGROUND gcontext))
	 (bg (GCONTEXT-FOREGROUND gcontext)))
     (WITH-GCONTEXT (gcontext :foreground fg :background bg)
       (DRAW-IMAGE-GLYPHS
	 (gesture-window gesture)
	 gcontext
	 (round (- (gesture-width gesture)
		   (gesture-title-width gesture)) 2)	;start x
	 baseline-y				;start y
	 (gesture-title gesture))))))
   #|
   ;; Show each menu item (position is relative to item window)
   (dolist (item (menu-item-alist menu))
     (DRAW-IMAGE-GLYPHS
       (first item) gcontext
       0					;start x
       baseline-y				;start y
       (second item)))))



  (mapcar #'princ 
          `(" x " x " y " y " gesture-width" ,(gesture-width gesture)))
|#
(defun gesture-present (gesture x y)
  
  ;; Try to center first item at the given location, but
  ;; make sure menu is completely visible in its parent

  (let ((gesture-window (gesture-window gesture)))
    (multiple-value-bind (tree parent) (QUERY-TREE gesture-window)
      (declare (ignore tree))
      (WITH-STATE (parent)
	(let* ((parent-width  (DRAWABLE-WIDTH parent))
	       (parent-height (DRAWABLE-HEIGHT parent))
	       (gesture-height   (+ *gesture-item-margin*
                                    500))
	       (gesture-x        (max 0 (min (- parent-width (gesture-width gesture))
					  (- x (gesture-width gesture)))))
	       (gesture-y        (max 1 (min (- parent-height gesture-height)
					  (- y  20)))))
;  (mapcar #'princ 
;          `( "gesture-x " ,gesture-x " gesture-y " ,gesture-y))
;          `(" x " x " y " y " gesture-width" ,(gesture-width gesture)))

	  (WITH-STATE (gesture-window)
	    (setf (DRAWABLE-X gesture-window) gesture-x
		  (DRAWABLE-Y gesture-window) gesture-y)))))

    ;; Make menu visible
    (MAP-WINDOW gesture-window)))



(defun start-gesture-track (&optional (font-name "fixed"))
  (let* ((display   (open-default-display))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (nice-font (OPEN-FONT display font-name))
	 (a-gesture    (create-gesture (screen-root screen)	
                                        ;the menu's parent
				 fg-color bg-color nice-font)))
    
    (setf (gesture-title a-gesture) "Please pick your favorite language:")
    
    ;; Bedevil the user until he picks a nice programming language
    (unwind-protect
	(do (choice)
	    ((and (setf choice (event-loop2 a-gesture 300 100))
		  (string-equal "Lisp" choice))))

      (CLOSE-DISPLAY display))))


(defun event-loop2 (gesture x y)
  ;; Display the menu so that first item is at x,y.
  (gesture-present gesture  x y)
  
    ;; Event processing loop
    (do () (t)				
      (EVENT-CASE ((DRAWABLE-DISPLAY (gesture-window gesture)) :force-output-p t)
	(:exposure     (count)
		       
	 (when (zerop count) (gesture-refresh gesture))
	 ;; Discard all but final :exposure then display the menu
	 t)

	(:button-press  (x y)
                        (princ  x )
                        (princ " ")
                        (princ y )
                        (princ " ")
                        (gesture-draw-point gesture x y)
                        nil
                        ) 

	(:motion-notify  (window x y code)
                        (gesture-draw-point gesture x y)
                        nil
                        )
))
    ;; Erase the menu
    (UNMAP-WINDOW (gesture-window gesture))
    "foo"
)
    


  
(defun gesture-draw-point (gesture x y)
    ;; Draw a point
    (DRAW-POINT (gesture-window gesture)
		    (gesture-gcontext gesture)
                    x y))



(defun pop-up (host strings &key (title "Pick one:") (font "fixed"))
  (let* ((display   (OPEN-DISPLAY host))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (font      (OPEN-FONT display font))
	 (parent-width 400)
	 (parent-height 400)
	 (parent    (CREATE-WINDOW :parent (SCREEN-ROOT screen)
				   :override-redirect :on
				   :x 100 :y 100
				   :width parent-width :height parent-height
				   :background bg-color
				   :event-mask (MAKE-EVENT-MASK :button-press
								:exposure)))
	 (a-menu    (create-menu parent fg-color bg-color font))
	 (prompt    "Press a button...")	 
	 (prompt-gc (CREATE-GCONTEXT :drawable parent
				     :foreground fg-color
				     :background bg-color
				     :font font))
	 (prompt-y  (FONT-ASCENT font))
	 (ack-y     (- parent-height  (FONT-DESCENT font))))
    
    (setf (menu-title a-menu) title)
    (apply #'menu-set-item-list a-menu strings)
    
    ;; Present main window
    (MAP-WINDOW parent)
    
    (flet ((display-centered-text
	     (window string gcontext height width)	     
	     (multiple-value-bind (w a d l r fa fd) (text-extents gcontext string)
	       (declare (ignore a d l r))
	       (let ((box-height (+ fa fd)))
		 
		 ;; Clear previous text
		 (CLEAR-AREA window
			     :x 0 :y (- height fa)
			     :width width :height box-height)
		 
		 ;; Draw new text
		 (DRAW-IMAGE-GLYPHS window gcontext (round (- width w) 2) height string)))))
      
      (unwind-protect
	  (loop
	    (EVENT-CASE (display :force-output-p t)
	      
	      (:exposure (count)
			 
			 ;; Display prompt
			 (when (zerop count)
			   (display-centered-text
			     parent
			     prompt
			     prompt-gc
			     prompt-y
			     parent-width))
			 t)
	      
	      (:button-press (x y)
			     
			     ;; Pop up the menu
			     (let ((choice (menu-choose a-menu x y)))
			       (if choice
				   (display-centered-text
				     parent
				     (format nil "You have selected ~a." choice)
				     prompt-gc
				     ack-y
				     parent-width)
				   
				   (display-centered-text
				     parent
				     "No selection...try again."
				     prompt-gc
				     ack-y
				     parent-width)))
			     t)	    	    
	      
	      (otherwise ()
			 ;;Ignore and discard any other event
			 t)))
	
	(CLOSE-DISPLAY display)))))

