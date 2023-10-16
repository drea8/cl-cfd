
(setq nx 80
      dx (/ 2 (- nx 1))
      nt 50
      dt .005
      c 1
      u '()
      un '())


(defun define-initial-conditions ()
  (setq u
	(loop :for n
	      :from 0
	      :to nx
	      :collect 1))
  (loop :for n
	:from 10
	:to 30
	:do (setf (nth n u) 2) ))

(setq time-range (loop for n from 20 to 25
		       collect n)
      timestep-range (loop for n from 0 to nt
			   collect n)
      i-range (loop for n from 1 to nx
		    collect n) )
      
(defun transpose-rh (un c dt dx i)
  (- (nth i un)
     (* c (/ dt  dx) (- (nth i un)
		     (nth (- i 1) un)))))

(defun transpose-rh-inviscid-burgers (un c dt dx i)
  (- (nth i un)
     (* (nth i un) (/ dt  dx) (- (nth i un)
		     (nth (- i 1) un)))))

 
(defun finite-difference ()
  (setq un
      (loop :for n
	    :from 0
	    :to nx
	    :collect 1))
  (loop :for step
	:in time-range
	:do
	(setq nt step)
	(loop :for n in timestep-range
	      :do
	      (setq un u)
	      (loop :for i :in i-range
		    :do
		    (setf (nth i u)
			  (transpose-rh un c dt dx i))))))
			       			    
(define-initial-conditions)
(finite-difference)  
