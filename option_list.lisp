(require :clx)
(use-package :xlib)

;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;; These functions demonstrate a simple menu implementation described in            |
;;; Kimbrough, Kerry, "Windows to the Future", Lisp Pointers, Oct-Nov, 1987.         |
;;; See functions JUST-SAY-LISP and POP-UP for demonstrations.                       |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+



(defstruct (ol-st)
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



(defun create-ol-st (parent-window text-color background-color text-font)
  (make-ol-st
    ;; Create ol-st graphics context
    :gcontext (CREATE-GCONTEXT :drawable   parent-window
			       :foreground text-color
			       :background background-color
			       :font       text-font)
    ;; Create ol-st window
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
		:event-mask   (MAKE-EVENT-MASK :exposure))))


(defun ol-st-set-item-list (ol-st &rest item-strings)
  ;; Assume the new items will change the ol-st's width and height
  (setf (ol-st-geometry-changed-p ol-st) t)

  ;; Destroy any existing item windows
  (dolist (item (ol-st-item-alist ol-st))
    (DESTROY-WINDOW (first item)))

  ;; Add (item-window item-string) elements to item-alist
  (setf (ol-st-item-alist ol-st)
	(let (alist)
	  (dolist (item item-strings (nreverse alist))
	    (push (list (CREATE-WINDOW
			  :parent     (ol-st-window ol-st)
			  :x          0         ;temporary value
			  :y          0         ;temporary value
			  :width      16        ;temporary value
			  :height     16        ;temporary value
			  :background (GCONTEXT-BACKGROUND (ol-st-gcontext ol-st)))
			item)
		  alist)))))

(defparameter *ol-st-item-margin* 4
  "Minimum number of pixels surrounding ol-st items.")


(defun ol-st-recompute-geometry (ol-st)
  (when (ol-st-geometry-changed-p ol-st)
    (let* ((ol-st-font   (GCONTEXT-FONT (ol-st-gcontext ol-st)))
	   (title-width (TEXT-EXTENTS ol-st-font (ol-st-title ol-st)))
	   (item-height (+ (FONT-ASCENT ol-st-font) (FONT-DESCENT ol-st-font)))
	   (item-width  0)
	   (items       (ol-st-item-alist ol-st))
	   ol-st-width)
      
      ;; Find max item string width
      (dolist (next-item items)
	(setf item-width (max item-width 
			      (TEXT-EXTENTS ol-st-font (second next-item)))))
      
      ;; Compute final ol-st width, taking margins into account
      (setf ol-st-width (max title-width
			    (+ item-width *ol-st-item-margin* *ol-st-item-margin*)))      
      (let ((window  (ol-st-window ol-st))
	    (delta-y (+ item-height *ol-st-item-margin*)))
	
	;; Update width and height of ol-st window        
	(WITH-STATE (window)
	  (setf (DRAWABLE-WIDTH  window) ol-st-width
		(DRAWABLE-HEIGHT window) (+ *ol-st-item-margin*
					    (* (1+ (length items))
					       delta-y))))
	
	;; Update width, height, position of item windows
	(let ((item-left     (round (- ol-st-width item-width) 2))
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
      (MAP-SUBWINDOWS (ol-st-window ol-st))

      ;; Save item geometry
      (setf (ol-st-item-width ol-st)         item-width
	    (ol-st-item-height ol-st)        item-height
	    (ol-st-width ol-st)              ol-st-width
	    (ol-st-title-width ol-st)        title-width
	    (ol-st-geometry-changed-p ol-st) nil))))


(defun ol-st-refresh (ol-st)
 (let* ((gcontext   (ol-st-gcontext ol-st))
        (baseline-y (FONT-ASCENT (GCONTEXT-FONT gcontext))))
   
   ;; Show title centered in "reverse-video"
   (let ((fg (GCONTEXT-BACKGROUND gcontext))
	 (bg (GCONTEXT-FOREGROUND gcontext)))
     (WITH-GCONTEXT (gcontext :foreground fg :background bg)
       (DRAW-IMAGE-GLYPHS
	 (ol-st-window ol-st)
	 gcontext
	 (round (- (ol-st-width ol-st)
		   (ol-st-title-width ol-st)) 2)	;start x
	 baseline-y				;start y
	 (ol-st-title ol-st))))
   
   ;; Show each ol-st item (position is relative to item window)
   (dolist (item (ol-st-item-alist ol-st))
     (DRAW-IMAGE-GLYPHS
       (first item) gcontext
       0					;start x
       baseline-y				;start y
       (second item)))))


(defun ol-st-choose (ol-st x y)
  ;; Display the ol-st so that first item is at x,y.
  (ol-st-present ol-st x y)
  
  (let ((items (ol-st-item-alist ol-st))
	(mw    (ol-st-window ol-st))
	selected-item)

    ;; Event processing loop
    (do () (selected-item)				
      (EVENT-CASE ((DRAWABLE-DISPLAY mw) :force-output-p t)
	(:exposure     (count)
		       
	 ;; Discard all but final :exposure then display the ol-st
	 (when (zerop count) (ol-st-refresh ol-st))
	 t)

	(otherwise ()
		   ;;Ignore and discard any other event
		   t)))
    
    ;; Erase the ol-st
    (UNMAP-WINDOW mw)
    
    ;; Return selected item string, if any
    (unless (eq selected-item :none) selected-item)))


(defun ol-st-highlight-item (ol-st position)
  (let* ((box-margin  (round *ol-st-item-margin* 2))
	 (left        (- (round (- (ol-st-width ol-st) (ol-st-item-width ol-st)) 2)
			 box-margin))
	 (top         (- (* (+ *ol-st-item-margin* (ol-st-item-height ol-st))
			    (1+ position))
			 box-margin))
	 (width       (+ (ol-st-item-width ol-st) box-margin box-margin))
	 (height      (+ (ol-st-item-height ol-st) box-margin box-margin)))
    
    ;; Draw a box in ol-st window around the given item.
    (DRAW-RECTANGLE (ol-st-window ol-st)
		    (ol-st-gcontext ol-st)
		    left top
		    width height)))

(defun ol-st-unhighlight-item (ol-st position)
  ;; Draw a box in the ol-st background color
  (let ((gcontext (ol-st-gcontext ol-st)))
    (WITH-GCONTEXT (gcontext :foreground (gcontext-background gcontext))
      (ol-st-highlight-item ol-st position))))


(defun ol-st-present (ol-st x y)
  ;; Make sure ol-st geometry is up-to-date
  (ol-st-recompute-geometry ol-st)
  
  ;; Try to center first item at the given location, but
  ;; make sure ol-st is completely visible in its parent
  (let ((ol-st-window (ol-st-window ol-st)))
    (multiple-value-bind (tree parent) (QUERY-TREE ol-st-window)
      (declare (ignore tree))
      (WITH-STATE (parent)
	(let* ((parent-width  (DRAWABLE-WIDTH parent))
	       (parent-height (DRAWABLE-HEIGHT parent))
	       (ol-st-height   (+ *ol-st-item-margin*
				 (* (1+ (length (ol-st-item-alist ol-st)))
				    (+ (ol-st-item-height ol-st)  *ol-st-item-margin*))))
	       (ol-st-x        (max 0 (min (- parent-width (ol-st-width ol-st))
					  (- x (round (ol-st-width ol-st) 2)))))
	       (ol-st-y        (max 0 (min (- parent-height ol-st-height)
					  (- y (round (ol-st-item-height ol-st) 2/3)
					     *ol-st-item-margin*)))))
	  (WITH-STATE (ol-st-window)
	    (setf (DRAWABLE-X ol-st-window) ol-st-x
		  (DRAWABLE-Y ol-st-window) ol-st-y)))))

    ;; Make ol-st visible
    (MAP-WINDOW ol-st-window)))


(defun show-option-list (options x y  &optional (font-name "fixed"))
  (let* ((display   (open-default-display))
	 (screen    (first (DISPLAY-ROOTS display)))
	 (fg-color  (SCREEN-BLACK-PIXEL screen))
	 (bg-color  (SCREEN-WHITE-PIXEL screen))
	 (nice-font (OPEN-FONT display font-name))
	 (a-ol-st    (create-ol-st (screen-root screen)	;the ol-st's parent
				 fg-color bg-color nice-font)))
    
    (setf (ol-st-title a-ol-st) "Please pick your favorite language:")
    (apply #'ol-st-set-item-list a-ol-st options)
    
    ;; Bedevil the user until he picks a nice programming language

    (unwind-protect
         (ol-st-choose a-ol-st x y))

      (CLOSE-DISPLAY display))))