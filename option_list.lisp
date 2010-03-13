(require :clx)
(use-package :xlib)

;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;; These functions demonstrate a simple menu implementation described in            |
;;; Kimbrough, Kerry, "Windows to the Future", Lisp Pointers, Oct-Nov, 1987.         |
;;; See functions JUST-SAY-LISP and POP-UP for demonstrations.                       |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+



(defstruct (option-list-struct)
  "A simple menu of text strings."
  (title "choose an item:")
  item-alist					;((item-window item-string))
  window
  gcontext
  width
  title-width
  item-width
  item-height
  (geometry-changed-p t))			;nil iff unchanged since displayed



(defun create-option-list-struct (parent-window text-color background-color text-font)
  (make-option-list-struct
    ;; Create option-list-struct graphics context
    :gcontext (CREATE-GCONTEXT :drawable   parent-window
			       :foreground text-color
			       :background background-color
			       :font       text-font)
    ;; Create option-list-struct window
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
		:event-mask   (MAKE-EVENT-MASK :leave-window					       
					       :exposure))))


(defun option-list-struct-set-item-list (option-list-struct &rest item-strings)
  ;; Assume the new items will change the option-list-struct's width and height
  (setf (option-list-struct-geometry-changed-p option-list-struct) t)

  ;; Destroy any existing item windows
  (dolist (item (option-list-struct-item-alist option-list-struct))
    (DESTROY-WINDOW (first item)))

  ;; Add (item-window item-string) elements to item-alist
  (setf (option-list-struct-item-alist option-list-struct)
	(let (alist)
	  (dolist (item item-strings (nreverse alist))
	    (push (list (CREATE-WINDOW
			  :parent     (option-list-struct-window option-list-struct)
			  :x          0         ;temporary value
			  :y          0         ;temporary value
			  :width      16        ;temporary value
			  :height     16        ;temporary value
			  :background (GCONTEXT-BACKGROUND (option-list-struct-gcontext option-list-struct))
			  :event-mask (MAKE-EVENT-MASK :enter-window
						       :leave-window
						       :button-press
						       :button-release))
			item)
		  alist)))))

(defparameter *option-list-struct-item-margin* 4
  "Minimum number of pixels surrounding option-list-struct items.")


(defun option-list-struct-recompute-geometry (option-list-struct)
  (when (option-list-struct-geometry-changed-p option-list-struct)
    (let* ((option-list-struct-font   (GCONTEXT-FONT (option-list-struct-gcontext option-list-struct)))
	   (title-width (TEXT-EXTENTS option-list-struct-font (option-list-struct-title option-list-struct)))
	   (item-height (+ (FONT-ASCENT option-list-struct-font) (FONT-DESCENT option-list-struct-font)))
	   (item-width  0)
	   (items       (option-list-struct-item-alist option-list-struct))
	   option-list-struct-width)
      
      ;; Find max item string width
      (dolist (next-item items)
	(setf item-width (max item-width 
			      (TEXT-EXTENTS option-list-struct-font (second next-item)))))
      
      ;; Compute final option-list-struct width, taking margins into account
      (setf option-list-struct-width (max title-width
			    (+ item-width *option-list-struct-item-margin* *option-list-struct-item-margin*)))      
      (let ((window  (option-list-struct-window option-list-struct))
	    (delta-y (+ item-height *option-list-struct-item-margin*)))
	
	;; Update width and height of option-list-struct window        
	(WITH-STATE (window)
	  (setf (DRAWABLE-WIDTH  window) option-list-struct-width
		(DRAWABLE-HEIGHT window) (+ *option-list-struct-item-margin*
					    (* (1+ (length items))
					       delta-y))))
	
	;; Update width, height, position of item windows
	(let ((item-left     (round (- option-list-struct-width item-width) 2))
	      (next-item-top delta-y))
	  (dolist (next-item items)
	    (let ((window (first next-item)))
	      (WITH-STATE (window)
		(setf (DRAWABLE-HEIGHT window) item-height
		      (DRAWABLE-WIDTH  window) item-width
		      (DRAWABLE-X      window) item-left
		      (DRAWABLE-Y      window) next-item-top)))
	    (incf next-item-top delta-y))))
      
      ;; Map all item windows
      (MAP-SUBWINDOWS (option-list-struct-window option-list-struct))

      ;; Save item geometry
      (setf (option-list-struct-item-width option-list-struct)         item-width
	    (option-list-struct-item-height option-list-struct)        item-height
	    (option-list-struct-width option-list-struct)              option-list-struct-width
	    (option-list-struct-title-width option-list-struct)        title-width
	    (option-list-struct-geometry-changed-p option-list-struct) nil))))


(defun option-list-struct-refresh (option-list-struct)
 (let* ((gcontext   (option-list-struct-gcontext option-list-struct))
        (baseline-y (FONT-ASCENT (GCONTEXT-FONT gcontext))))
   
   ;; Show title centered in "reverse-video"
   (let ((fg (GCONTEXT-BACKGROUND gcontext))
	 (bg (GCONTEXT-FOREGROUND gcontext)))
     (WITH-GCONTEXT (gcontext :foreground fg :background bg)
       (DRAW-IMAGE-GLYPHS
	 (option-list-struct-window option-list-struct)
	 gcontext
	 (round (- (option-list-struct-width option-list-struct)
		   (option-list-struct-title-width option-list-struct)) 2)	;start x
	 baseline-y				;start y
	 (option-list-struct-title option-list-struct))))
   
   ;; Show each option-list-struct item (position is relative to item window)
   (dolist (item (option-list-struct-item-alist option-list-struct))
     (DRAW-IMAGE-GLYPHS
       (first item) gcontext
       0					;start x
       baseline-y				;start y
       (second item)))))


(defun option-list-struct-choose (option-list-struct x y)
  ;; Display the option-list-struct so that first item is at x,y.
  (option-list-struct-present option-list-struct x y)
  
  (let ((items (option-list-struct-item-alist option-list-struct))
	(mw    (option-list-struct-window option-list-struct))
	selected-item)

    ;; Event processing loop
    (do () (selected-item)				
      (EVENT-CASE ((DRAWABLE-DISPLAY mw) :force-output-p t)
	(:exposure     (count)
		       
	 ;; Discard all but final :exposure then display the option-list-struct
	 (when (zerop count) (option-list-struct-refresh option-list-struct))
	 t)
	
	(:button-release (event-window)
	 ;;Select an item
	 (setf selected-item (second (assoc event-window items)))
	 t)
	
	(:enter-notify (window)
	 ;;Highlight an item
	 (let ((position (position window items :key #'first)))
	   (when position
	     (option-list-struct-highlight-item option-list-struct position)))
	 t)
	
	(:leave-notify (window kind)
	 (if (eql mw window)
	     ;; Quit if pointer moved out of main option-list-struct window
	     (setf selected-item (when (eq kind :ancestor) :none))

	   ;; Otherwise, unhighlight the item window left
	   (let ((position (position window items :key #'first)))
	     (when position
	       (option-list-struct-unhighlight-item option-list-struct position))))
	 t)
	
	(otherwise ()
		   ;;Ignore and discard any other event
		   t)))
    
    ;; Erase the option-list-struct
    (UNMAP-WINDOW mw)
    
    ;; Return selected item string, if any
    (unless (eq selected-item :none) selected-item)))


(defun option-list-struct-highlight-item (option-list-struct position)
  (let* ((box-margin  (round *option-list-struct-item-margin* 2))
	 (left        (- (round (- (option-list-struct-width option-list-struct) (option-list-struct-item-width option-list-struct)) 2)
			 box-margin))
	 (top         (- (* (+ *option-list-struct-item-margin* (option-list-struct-item-height option-list-struct))
			    (1+ position))
			 box-margin))
	 (width       (+ (option-list-struct-item-width option-list-struct) box-margin box-margin))
	 (height      (+ (option-list-struct-item-height option-list-struct) box-margin box-margin)))
    
    ;; Draw a box in option-list-struct window around the given item.
    (DRAW-RECTANGLE (option-list-struct-window option-list-struct)
		    (option-list-struct-gcontext option-list-struct)
		    left top
		    width height)))

(defun option-list-struct-unhighlight-item (option-list-struct position)
  ;; Draw a box in the option-list-struct background color
  (let ((gcontext (option-list-struct-gcontext option-list-struct)))
    (WITH-GCONTEXT (gcontext :foreground (gcontext-background gcontext))
      (option-list-struct-highlight-item option-list-struct position))))


(defun option-list-struct-present (option-list-struct x y)
  ;; Make sure option-list-struct geometry is up-to-date
  (option-list-struct-recompute-geometry option-list-struct)
  
  ;; Try to center first item at the given location, but
  ;; make sure option-list-struct is completely visible in its parent
  (let ((option-list-struct-window (option-list-struct-window option-list-struct)))
    (multiple-value-bind (tree parent) (QUERY-TREE option-list-struct-window)
      (declare (ignore tree))
      (WITH-STATE (parent)
	(let* ((parent-width  (DRAWABLE-WIDTH parent))
	       (parent-height (DRAWABLE-HEIGHT parent))
	       (option-list-struct-height   (+ *option-list-struct-item-margin*
				 (* (1+ (length (option-list-struct-item-alist option-list-struct)))
				    (+ (option-list-struct-item-height option-list-struct)  *option-list-struct-item-margin*))))
	       (option-list-struct-x        (max 0 (min (- parent-width (option-list-struct-width option-list-struct))
					  (- x (round (option-list-struct-width option-list-struct) 2)))))
	       (option-list-struct-y        (max 0 (min (- parent-height option-list-struct-height)
					  (- y (round (option-list-struct-item-height option-list-struct) 2/3)
					     *option-list-struct-item-margin*)))))
	  (WITH-STATE (option-list-struct-window)
	    (setf (DRAWABLE-X option-list-struct-window) option-list-struct-x
		  (DRAWABLE-Y option-list-struct-window) option-list-struct-y)))))

    ;; Make option-list-struct visible
    (MAP-WINDOW option-list-struct-window)))

(defun just-say-lisp (&optional (font-name "fixed"))
  (let* ((display   (open-default-display))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (nice-font (OPEN-FONT display font-name))
	 (a-option-list-struct    (create-option-list-struct (screen-root screen)	;the option-list-struct's parent
				 fg-color bg-color nice-font)))
    
    (setf (option-list-struct-title a-option-list-struct) "Please pick your favorite language:")
    (option-list-struct-set-item-list a-option-list-struct "Fortran" "APL" "Forth" "Lisp")
    
    ;; Bedevil the user until he picks a nice programming language
    (unwind-protect
	(do (choice)
	    ((and (setf choice (option-list-struct-choose a-option-list-struct 2000 100))
		  (string-equal "Lisp" choice))))

      (CLOSE-DISPLAY display))))
  

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
	 (a-option-list-struct    (create-option-list-struct parent fg-color bg-color font))
	 (prompt    "Press a button...")	 
	 (prompt-gc (CREATE-GCONTEXT :drawable parent
				     :foreground fg-color
				     :background bg-color
				     :font font))
	 (prompt-y  (FONT-ASCENT font))
	 (ack-y     (- parent-height  (FONT-DESCENT font))))
    
    (setf (option-list-struct-title a-option-list-struct) title)
    (apply #'option-list-struct-set-item-list a-option-list-struct strings)
    
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
			     
			     ;; Pop up the option-list-struct
			     (let ((choice (option-list-struct-choose a-option-list-struct x y)))
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

