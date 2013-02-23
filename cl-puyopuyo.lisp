(defpackage :cl-puyopuyo
  (:use :common-lisp :quicklisp )
  (:export #:cl-puyopuyo))

(in-package :cl-puyopuyo)

;;width/height of bitmaos is 32x32
(defconstant +puyo-width+ 32)
(defconstant +puyo-height+ 32)

(defconstant +field-width+ 6)
(defconstant +field-height+ 12)

(defvar *max-cols* 4)

(defvar *points* 0)
(defvar *level* 1)

(defparameter *field* (make-array (* +field-width+ +field-height+)))
(defparameter *solve-field* (make-array (* +field-width+ +field-height+)))

(defvar *state* nil)
(defvar *run* nil)
(defparameter *puyos* (make-list 0))

;;a puyo consists of colors 0-3 (0=blue, 1=red, 2=green and 3=yellow), a state (dropping or landed) and a position
(defstruct puyo
  x
  y
  col
  state) ;;dropping / landed

;;we need two stones, 1st and 2nd
;;these need to be global, so that the dropPuyo-thread can access the stones
(defvar *first* nil)
(defvar *second* nil)

;;sdl surface pictures for blitting
(defparameter *blue* nil)
(defparameter *red* nil)
(defparameter *green* nil)
(defparameter *yellow* nil)
(defparameter *gameover* nil)


(defun clear-field ()
  (dotimes (y +field-height+)
    (dotimes (x +field-width+)
      (setf (aref *field* (getOffset x y)) -1))))

(defun print-field ()
  (dotimes (y +field-height+)
    (dotimes (x +field-width+)
      (format t "~D " (aref *field* (getOffset x y))))
    (terpri)))

(defun count-elements (f elem)
  (count elem f :test #'=))

(defun copy-field (src dst)
  (dotimes(i (length src))
    (setf (aref dst i) (aref src i))))

(defun number-to-color (number)
  (case number
    (0 *blue*)
    (1 *red*)
    (2 *green*)
    (3 *yellow*)))

(defun draw-field (f)
  ;;i guess there is a sneaky lispy way to cut this down somehow
  (dotimes (y +field-height+)
    (dotimes (x +field-width+)
      (let ((col (aref f (getOffset x y))))
        (lispbuilder-sdl:draw-surface-at-*
         (number-to-color col)
         (* x +puyo-width+)
         (* y +puyo-height+)
         :surface lispbuilder-sdl:*default-display*)))))

(defun moveToLeft (f s)
  (setf *state* 'pause)
  (let (
	(fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x))
	(fstate (slot-value f 'state))
	(sstate (slot-value s 'state)))
    (if (and (eq sstate 'dropping) (eq fstate 'dropping))
	(cond 
	  ;;stones lay on each other
	  ((= fx sx)
	   (if (and (= (aref *field* (getoffset (- fx 1) fy)) -1) (>= (- fx 1) 0)
		    (= (aref *field* (getoffset (- sx 1) sy)) -1) (>= (- sx 1) 0))
	       (progn
		 (setf (slot-value f 'x) (- fx 1))
		 (setf (slot-value s 'x) (- sx 1)))))
	  ;;s is left from f
	  ((< sx fx)
	   (if (and (= (aref *field* (getoffset (- sx 1) sy)) -1) (>= (- sx 1) 0))
	       (progn
		 (setf (slot-value s 'x) (- sx 1))
		 (setf (slot-value f 'x) (- fx 1)))))
	  ;;f is left from s
	  ((< fx sx)
	     (if (and (= (aref *field* (getoffset (- fx 1) fy)) -1) (>= (- fx 1) 0))
		 (progn
		   (setf (slot-value f 'x) (- fx 1))	       
		   (setf (slot-value s 'x) (- sx 1))))))))
  (setf *state* 'unpause))

(defun moveToRight (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x))
	(fstate (slot-value f 'state))
	(sstate (slot-value s 'state)))
    (if (and (eq sstate 'dropping) (eq fstate 'dropping))
	(cond 
	  ;;stones lay on each other
	  ((= fx sx)
	   (progn
	     (if (and
		  (= (aref *field* (getoffset (+ fx 1) fy)) -1) (< (+ fx 1 ) +field-width+)
		  (= (aref *field* (getoffset (+ sx 1) sy)) -1) (< (+ sx 1 ) +field-width+))
		 (progn
		   (setf (slot-value f 'x) (+ fx 1))
		   (setf (slot-value s 'x) (+ sx 1))))))
	  ;;fx is left from sx
	  ((< fx sx)
	   (progn
	     (if (and (= (aref *field* (getoffset (+ sx 1) sy)) -1) (< (+ sx 1) +field-width+))
		 (progn
		   (setf (slot-value s 'x) (+ sx 1)) 
		   (setf (slot-value f 'x) (+ fx 1))))))	       
	  ;;sx is left from sx
	  ((< sx fx)
	   (progn
	     (if (and (= (aref *field* (getoffset (+ fx 1) fy)) -1) (< (+ fx 1) +field-width+))
		 (progn
		   (setf (slot-value f 'x) (+ fx 1))
		   (setf (slot-value s 'x) (+ sx 1)))))))))
  (setf *state* 'unpause))


(defun rotateStones (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    (cond
      ((< fx sx)
       (if (/= sy 0)
	   (progn
	     (setf (slot-value f 'y) (- sy 1))
	     (setf (slot-value f 'x) sx))
	   (progn
	     (setf (slot-value f 'y) (+ sy 1))
	     (setf (slot-value f 'x) sx))))
      ((< fy sy)(= fx sx)
       (if (= fx 0)
	   (progn
	     (setf (slot-value f 'x) (+ fx 1))
	     (setf (slot-value f 'y) (+ fy 1)))
	   (progn
	     (setf (slot-value s 'y) (- sy 1))
	     (setf (slot-value s 'x) (- sx 1)))))
      ((> fx sx) (= fy sy)
       (if (/= sy 0)
	   (progn
	     (setf (slot-value s 'x) (+ sx 1))
	     (setf (slot-value s 'y) (- sy 1)))
	   (progn
	     (setf (slot-value s 'x) (+ sx 1))
	     (setf (slot-value f 'y) (+ fy 1)))))
      ((> fy sy)(= fx sx)
       (if (= fx 0)
	   (progn
	     (setf (slot-value s 'x) (+ sx 1))
	     (setf (slot-value s 'y) (+ sy 1)))
	   (progn
	     (setf (slot-value s 'y) (+ sy 1))
	     (setf (slot-value f 'x) (- sx 1)))))))
  (setf *state* 'unpause))

(defun getOffset (x y)
  (+ (* y +field-width+) x))

(defun dropPuyos ()
  (setq *puyos* (sort *puyos* #'> :key #'(lambda (x) (slot-value x 'y))))

  (loop for p in *puyos* do
       (if (eq (slot-value p 'state) 'dropping)
	   (if (and (< (+ (slot-value p 'y) 1) +field-height+)
		    (= (aref *field* (getOffset (slot-value p 'x) (+ (slot-value p 'y) 1))) -1))
	       (setf (slot-value p 'y) (+ (slot-value p 'y) 1))
	       (setf (slot-value p 'state) 'landed)))
       (puyosToField *puyos* *field*))
  
  (let ((landed 0))
    (loop for p in *puyos* do
  	 (if (eq (slot-value p 'state) 'landed)
  	     (incf landed)))
    (if (eq landed (length *puyos*))
	0
	1)))

(defun backtrack (f x y col)
  (if (and (>= x 0) (>= y 0) (< x +field-width+) (< y +field-height+))
      (if (= (aref f (getOffset x y)) col)
	  (progn
	    (setf (aref f (getOffset x y)) 255)
	    (if (or
		 (backtrack f (+ x 1) y col)
		 (backtrack f x (+ y 1) col)
		 (backtrack f (- x 1) y col)
		 (backtrack f x (- y 1) col)
		 nil)
		nil))
	  nil)))

(defun sweeping ()
  (dotimes (i (length *solve-field*))
    (when (= (aref *solve-field* i) 255)
      (setf *points* (+ *points* 2))	  
      (when (< *level* 10)	      
        (when (zerop (mod *points* 50))
          (incf *level*)))

      (setf (aref *field* i) -1)))

  (dolist (p *puyos*)
    (pop *puyos*))
  
  (fieldToPuyos)

  (loop do
       (backtrackPuyos)
       ;; (puyosToField *puyos* *field*)
       ;; (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
       ;; (draw-field *field*)
       ;; (lispbuilder-sdl:update-display)
       ;; (sleep 1)
     while (= (dropPuyos) 1)))

(defun dropPuyo-thread ()
  (loop do
    (unless (or (eq *state* 'computing) (eq *state* 'pause))
      (sleep (- 1 (/ *level* 10)))
      (if (eq (dropPuyos) 0)
          (setf *state* 'newPuyos)))
     while (= 1 *run*)))

(defun fieldToPuyos ()
  "this helper function will take a field->f and fill the lisp of puyos->p. this is only for testing purpose"
  (loop for y from 0 to (- +field-height+ 1)  do
       (loop for x from 0 to (- +field-width+ 1) do	    
	    (if (/= (aref *field* (getOffset x y)) -1)
		(progn
		  (push (make-puyo :x x :y y :col (aref *field* (getOffset x y)) :state 'dropping) *puyos*))))))

(defun puyosToField (puyos f)
  "this helper function will take a list of puyos->p and place them into a field ->f"
  (loop for y from 0 to (- +field-height+ 1)  do
       (loop for x from 0 to (- +field-width+ 1) do	    
	    (setf (aref f (getOffset x y)) -1)))
  (loop for p in puyos do
       (setf (aref f (getOffset (slot-value p 'x) (slot-value p 'y))) (slot-value p 'col))))

(defun fileToField (f)
  "this helper function will read in a file called test.txt and construct a playing field->f. this is for testing purposes..."
  (let ((in (open "test.txt"))
	(l 0)
	(data 0))    
    (dotimes (i (length f))
      (setf data (digit-char-p (read-char in)))
      (unless data
        (setf data -1))
      (setf (aref f i) data) 
      (incf l)
      (when (= l 6)
        (read-char in)
        (setf l 0)))
    (close in)))

(defun backtrackPuyos ()
  (dolist (p *puyos*)
    (let ((col (slot-value p 'col))
          (x (slot-value p 'x))
          (y (slot-value p 'y)))
      (when (member col '(0 1 2 3))        
        (copy-field *field* *solve-field*)
        (backtrack *solve-field* x y col)
        (if (>= (count-elements *solve-field* 255) 4)
            (sweeping))))))

;;start / main
(defun main ()
  ;;is this sufficient to generate random colors/numbers ?
  (setf *random-state* (make-random-state t))

  ;;test stuff
  ;;(setf *first*  (make-puyo :x 3 :y 1 :col 1 :state 'dropping))
  ;;(setf *second* (make-puyo :x 3 :y 2 :col 2 :state 'dropping))

  (setf *first*  (make-puyo :x 2
                            :y 0
                            :col (random *max-cols*)
                            :state 'dropping))
  (setf *second* (make-puyo :x 3
                            :y 0
                            :col (random *max-cols*)
                            :state 'dropping))
  
  (setf *level* 0)
  (setf *points* 0)
  (setf *run* 1)
  (setf *state* 'unpause)
  
  (push *first* *puyos*)
  (push *second* *puyos*)
  
  ;;sdl
  (lispbuilder-sdl:init-video)
  (lispbuilder-sdl:initialise-default-font sdl:*ttf-font-vera*)
  
  
  ;;the puyo sprites are 32x32 pixels, so the playfield is 6*32 wide  and 12*32 heigh
  ;;we need double with, for displaying points and next puyos right from the playfield
					;(SETF (SDL:FRAME-RATE) 60)
  (lispbuilder-sdl:window  (* 2 +puyo-width+ +field-width+)
                           (* +puyo-height+ +field-height+))
  
  (lispbuilder-sdl:init-subsystems lispbuilder-sdl:sdl-init-timer)
  (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
  
  (clear-field)
  (bordeaux-threads:make-thread #'dropPuyo-thread :name "dropPuyo-thread")

  ;;test stuff
  ;;(fileToField *field*)
  ;;(fieldToPuyos)
  
  ;;sdl surface pictures for blitting
  (setf *blue* (lispbuilder-sdl:load-image "puyo_blue.png")
        *red* (lispbuilder-sdl:load-image "puyo_red.png")
        *green* (lispbuilder-sdl:load-image "puyo_green.png")
        *yellow* (lispbuilder-sdl:load-image "puyo_yellow.png")
        *gameover* (lispbuilder-sdl:load-image "gameover.png"))


  ;;game-loop
  (lispbuilder-sdl:with-events (:poll)
    (:quit-event () T)
    (:key-down-event (:key key)
		     (when (lispbuilder-sdl:key= key :sdl-key-escape)
		       (lispbuilder-sdl:push-quit-event))
		     (when (lispbuilder-sdl:key= key :sdl-key-p)
		       ;;pause handling
		       (cond ((eq *state* 'pause)
			      (setf *state* 'unpause))
			     ((eq *state* 'unpause)
			      (setf *state* 'pause))))
		     (when (lispbuilder-sdl:key= key :sdl-key-up)
		       (rotateStones *first* *second*)
		       (puyosToField *puyos* *field*))
		     (when (lispbuilder-sdl:key= key :sdl-key-down)
		       (dropPuyos))
		     (when (lispbuilder-sdl:key= key :sdl-key-left)
		       (movetoLeft *first* *second*)
		       (puyosToField *puyos* *field*))
		     (when (lispbuilder-sdl:key= key :sdl-key-right)
		       (moveToRight *first* *second*)
		       (puyosToField *puyos* *field*)))

    (:idle ()
	   (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
	   (lispbuilder-sdl:draw-string-solid-*
            "Puyopuyo"
            (+ 20 (* +field-width+ +puyo-width+))
            10
            :color (sdl:color :r 0 :g 0 :b 0))
	   (lispbuilder-sdl-gfx:draw-line-*
            (* +field-width+ +puyo-width+) 0
            (* +field-width+ +puyo-width+) (* +field-height+ +puyo-height+)
            :surface sdl:*default-display*
            :color (sdl:color :r 0 :g 0 :b 0))
	   
	   (puyosToField *puyos* *field*)

	   (if (eq *run* 0)
	       (lispbuilder-sdl:draw-surface-at-* *gameover* 0 0 :surface lispbuilder-sdl:*default-display*)
	       (draw-field *field*))
	   
	   (if (eq *state* 'pause)
	       (lispbuilder-sdl:draw-string-solid-* (format nil "Paused") (+ (* +field-width+ +puyo-width+) 50) (/ (* +field-height+ +puyo-height+) 2) :color (sdl:color :r 0 :g 0 :b 0))
	       (progn
		 (lispbuilder-sdl:draw-string-solid-* (format nil "~D" *points*) (+ (* +field-width+ +puyo-width+) 80) (/ (* +field-height+ +puyo-height+) 2) :color (sdl:color :r 0 :g 0 :b 0))
		 (lispbuilder-sdl:draw-string-solid-* (format nil "Level ~D" *level*) (+ (* +field-width+ +puyo-width+) 25) (+ (/ (* +field-height+ +puyo-height+) 2) 50) :color (sdl:color :r 0 :g 0 :b 0))))
	   
	   (lispbuilder-sdl:update-display)
	   (when (eq *state* 'newPuyos)
             (if (or (/= (aref *field* (getOffset 2 0)) -1)
                     (/= (aref *field* (getOffset 3 0)) -1))
                 (setf *run* 0)
                 (progn
                   (backtrackPuyos)
                   ;; we need to create new objects. what is the
                   ;; impact on memory / gc
                   (setf *first*  (make-puyo :x 2
                                             :y 0
                                             :col (random *max-cols*)
                                             :state 'dropping))
                   (setf *second*  (make-puyo :x 3
                                              :y 0
                                              :col (random *max-cols*)
                                              :state 'dropping))
                   
                   (push *first* *puyos*)
                   (push *second* *puyos*)
                   (setf *state* 'unpause)))))))



(main)
;;when esp ist pressed we drop out here. set run to zero so that the therad stops
;;we are done. bye bye
(setf *run* 0)
(sleep 1)
(format t "~%we are done...")
(lispbuilder-sdl:quit-sdl)



