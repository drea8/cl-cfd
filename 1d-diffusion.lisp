(setq
 nx 100
 dx (/ 2 (- nx 1)) 
 nt 20 ;; number of timtesteps to calculate
 nu 0.3 ;; viscosity
 sigma .2
 dt (* sigma (/ (expt dx 2) nu))
 )


(defun initial-conditions ()
  (loop for i from 0 to nx collect
	(cond ((and (> i 25) (< i 75)) 1)
	      (t 0))
	))
		      

(defun transpose-diffusion (un nu dt dx i)
  (+ (nth i un)
     (/ (* nu dt
	   (- (+ (nth (+ 1 i) un)
	         (nth (- i 1) un))
	      (* 2 (nth i un))) )
	(expt dx 2))))

(setq u (initial-conditions))

(defun finite-difference ()  
  (loop for n from 0 to nt do
	(setq un u)
	(loop for i from 1 to (- nx 1) do
	      (setf (nth i u)
		    (transpose-diffusion un nu dt dx i)))
	(print u)
	))


(finite-difference)
