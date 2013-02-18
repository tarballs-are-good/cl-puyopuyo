(defun updatePosition ()
  "threaded function which will update pos each second"
  (loop do
       (cond
	 ((eq *state* 'unpause)
	   (progn
	     (if (>= (slot-value *first* 'y) (slot-value *second* 'y))
		 (progn
		   (if (eq (slot-value *first* 'state) 'dropping)
		       (dropPuyo *first*))
		   (if (eq (slot-value *second* 'state) 'dropping)
		       (dropPuyo *second*)))
		 (progn
		   (if (eq (slot-value *second* 'state) 'dropping)
		       (dropPuyo *second*))
		   (if (eq (slot-value *first* 'state) 'dropping)
		       (dropPuyo *first*))))
	     (if (and 
		  (eq (slot-value *first* 'state) 'landed)
		  (eq (slot-value *second* 'state) 'landed))
		 (progn
		   ;;cl(os)/gc will destroy objects on its own
		   ;;(delete *first*)
		   ;;(delete *second*)
		   (setf *state* 'backtrack)))))
       
	 ((eq *state* 'sweeping)
	  (progn
	    (print "state sweeping::coordslist:")
	    (print *coordslist*)	     
	    (loop for p in *puyos* do
		 (setf (slot-value p 'state) 'dropping))
	    (dropPuyos *puyos*)
	    (setf *state* 'backtrack)))
	 
	 ((eq *state* 'backtrack)
	  (progn
	    (loop for puyo in *puyos* do
		 (copyField *field* *solveField*)
		 (backtrack *solveField* (slot-value puyo 'x) (slot-value puyo 'y) (slot-value puyo 'col))
		 (format t "backtrack found ~D matches" *matchStones*)
		 (if (>= *matchStones* 4)
		     (progn
		       (print "deleting matching stones")
		       (print "coordslist::")
		       (print *coordslist*)
		       (setf *state* 'sweeping)
		       (setf *matchStones* 0)
		       (loop for x in *coordsList* do
			    (setf (aref *field* (getOffset (car x) (cdr x))) -1)
			    (pop *coordsList*)
			    (setf *puyos* (delete x *puyos* :test #'equal :key #'(lambda (p) (cons (slot-value p 'x) (slot-value p 'y)))))))
		     (progn
		       ;;delete list -> there must be an easier way for this
		       (print "deleting coordlist again")
		       (loop for x in *coordsList* do
			    (pop *coordsList*))
		       (print "coordslist::")
		       (print *coordslist*)
		       (setf *matchStones* 0)
		       (setf *state* 'newPuyos))))))
	 
	 ((eq *state* 'newPuyos) 
	  (if (and (/= (aref *field* (getOffset 2 0)) -1) (/= (aref *field* (getOffset 3 0)) -1))
	      (progn		       
		(setf *run* 0)
		(print "GAME OVER"))
	      (progn
		(format t "~%make new puyos")
		;; we need to create new objects. what the impact on memory / gc
		(setf *first*  (make-puyo :x 2 :y 0 :col (random *maxCols*) :state 'dropping))
		(setf *second* (make-puyo :x 3 :y 0 :col (random *maxCols*) :state 'dropping))		   
		(setf *state* 'unpause)))))	 
	 (sleep .3)
	 while(= 1 *run*))
  (format t "~%updatePosition:end"))
	 