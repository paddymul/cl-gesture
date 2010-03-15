
(defclass point ()
  ((x :initform nil :initarg :x :accessor point-x)
   (y :initform nil :initarg :y :accessor point-y)))


(defmethod print-object ((object point) stream)
  (format stream "x:~D y:~D" (point-x object) (point-y object)))

(defmethod point-slope ((p1 point) (p2 point ))
  "returns the slope between two points dy/dx"
  (/  (- (point-x p1) (point-x p2))
      (let ((temp-denominator (- (point-y p1) (point-y p2))))
        (if (= temp-denominator 0)
            (+ 0.0000001 temp-denominator)
            temp-denominator))))

(defmethod simple-distance ((p1 point) (p2 point ))
  "returns the greatest distance along either axis between two points "
  (max (abs (- (point-x p1) (point-x p2)))
       (abs (- (point-y p1) (point-y p2)))))


(defmethod distance ((p1 point) (p2 point ))
  "returns the greatest distance along either axis between two points "
  (sqrt (+ (expt  (- (point-x p1) (point-x p2)) 2)
           (expt  (- (point-y p1) (point-y p2)))2 )))

(defmethod direction   ((p1 point) (p2 point ))
  "returns the direction from p1 to p2"
  (let ((ratio (point-slope p1 p2)))
    (if   (< 1 (abs ratio))
          (if (> (point-x p1) (point-x p2))
              :left
              :right)
          (if (> (point-y p1) (point-y p2))
              :up
              :down))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

                                        ; a series of one or more directions
(defclass gesture () 
  ((start-point                    :initarg :start-point :accessor start-point)
   (last-point      :initform nil  :initarg :last-point                          :reader last-point)
   (direction-chain :initform nil                                               :reader direction-chain)))

(defun make-gesture (start-point)
  (make-instance 'gesture :start-point start-point :last-point start-point))

(defmethod print-object ((object gesture) stream)
  (format stream "start-point:~A last-point:~A direction-chain:~A" 
          (start-point object) (last-point object) (direction-chain object)))
(defmethod print-object ((object gesture) stream)
  (format stream " direction-chain:~A" 
          (direction-chain object)))

(defmethod set-last-point ((g gesture) (p point))
  (when (>  (simple-distance (last-point g) p) 10)
    (let 
        ((dir (direction (last-point g) p)))
      (if (not (equal dir (car (direction-chain g))))
        (progn
          (push dir (slot-value g 'direction-chain))
          (setf (slot-value g 'last-point) p)
          dir)
        (progn
          (setf (slot-value g 'last-point) p)
          nil)))))



(defclass gesture-command ()
  ((direction-chain :initarg :direction-chain :accessor com-direction-chain)
   (val :initarg :val :accessor com-val)))

(defmethod print-object ((object gesture-command) stream)
  (format stream "<dir-seq ~A val ~A>" (com-direction-chain object) (com-val object)))


(defparameter *gc-list* '())

(defmacro make-gesture-command (direction-chain val)
  `(make-instance
    'gesture-command :direction-chain ',direction-chain :val ,val))

(defmacro def-gesture-command (direction val)
  `(push (make-gesture-command ,direction ,val) *gc-list*))


(defmethod gesture-command-applicable ((gc gesture-command) direction-list)
  "note: direction-list is expected to be reversed"
  (if 
   (equal 0 (search (reverse direction-list)  (com-direction-chain gc)) )
   gc
   nil))


(defmethod gesture-command-applicable ((gc gesture-command) (g gesture))
  (gesture-command-applicable gc (direction-chain g)))

(defun  find-applicable-gestures (g)
  "because of the magic of generic-functions g can be either a gesture
or a list of directions "
  (remove-if #'null (mapcar #'(lambda (gc) (gesture-command-applicable gc g)) *gc-list*)))


(defmethod gesture-posibilities ((g gesture))
  (let* ((d-chain (direction-chain g))
         (most-recent-direction (car (reverse d-chain))))
                   (mapcar 
                    #'(lambda (side) 
                        (cons side (list (find-applicable-gestures (cons side d-chain)))))
                    (remove most-recent-direction '(:left :right :up :down)))))