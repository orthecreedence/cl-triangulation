(defpackage :cl-triangulation
  (:use :cl)
  (:export :triangulate
           :polygon-clockwise-p
           :triangulation-loop-points))
(in-package :cl-triangulation)

(declaim (optimize (speed 3)
                   (safety 1)))

(define-condition triangulation-loop (error)
  ((points :initarg :points :reader triangulation-loop-points))
  (:report (lambda (c s)
             (let ((points (triangulation-loop-points c)))
               (format s "Triangulation entered infinite loop on ~a points ~a" (length points) points)))))

(defun normalize (x y)
  "Normalize a 2D vector"
  (let ((length (sqrt (+ (expt x 2) (expt y 2)))))
    (list (/ x length) (/ y length))))

(defun dot-prod (v1 v2)
  "Give the dot product of two 2D vectors."
  (+ (* (car v1) (car v2))
     (* (cadr v1) (cadr v2))))

(defun angle-between-three-points (p1 p2 p3)
  "Given three points, calculate the angle between p1 and p3 with p2 being the
  origin."
  (let* ((x2 (car p2))
         (y2 (cadr p2))
         (x1 (- (car p1) x2))
         (y1 (- (cadr p1) y2))
         (x3 (- (car p3) x2))
         (y3 (- (cadr p3) y2)))
    (let ((v1 (normalize x1 y1))
          (v2 (normalize x3 y3)))
      (let* ((theta-a (atan (car v1) (cadr v1)))
             (theta-b (atan (car v2) (cadr v2)))
             (theta (- theta-b theta-a)))
        (mod (* theta (/ 180 PI)) 360)))))

(defun lines-intersect-p (l1x1 l1y1 l1x2 l1y2 l2x1 l2y1 l2x2 l2y2)
  "Detects if two lines intersect."
  (declare (type float l1x1 l1y1 l1x2 l1y2 l2x1 l2y1 l2x2 l2y2))
  (let ((l1sx (- l1x2 l1x1))
        (l1sy (- l1y2 l1y1))
        (l2sx (- l2x2 l2x1))
        (l2sy (- l2y2 l2y1)))
    (let ((div (+ (* (- l2sx) l1sy) (* l1sx l2sy))))
      (when (zerop div)
        (return-from lines-intersect-p nil))
      (let ((ls (/ (+ (* (- l1sy) (- l1x1 l2x1))
                      (* l1sx (- l1y1 l2y1)))
                   div))
            (lt (/ (- (* l2sx (- l1y1 l2y1))
                      (* l2sy (- l1x1 l2x1)))
                   div)))
        ;(format t "ls, lt: ~a, ~a~%" ls lt)
        (and (not (or (and (= l1x1 l2x1) (= l1y1 l2y1))
                      (and (= l1x1 l2x2) (= l1y1 l2y2))
                      (and (= l1x2 l2x1) (= l1y2 l1y1))
                      (and (= l1x2 l2x2) (= l1y2 l2y2))))
             (< 0 ls 1)
             (< 0 lt 1))))))

(defun point-in-polygon-p (point polygon-points)
  "Tests if a point (x,y) is inside of a polygon."
  (declare (type list point)
           (type vector polygon-points))
  (let ((x (car point))
        (y (cadr point))
        (j (1- (length polygon-points)))
        (c nil))
    (dotimes (i (length polygon-points))
      (let ((px (car (aref polygon-points i)))
            (py (cadr (aref polygon-points i)))
            (pjx (car (aref polygon-points j)))
            (pjy (cadr (aref polygon-points j))))
        (when (or (and (= x px) (= y py))
                  (and (= x pjx) (= y pjy)))
          (return-from point-in-polygon-p t))
        (when (and (not (eq (> py y) (> pjy y)))
                   (< x (+ (/ (* (- pjx px) (- y py))
                              (- pjy py)) px)))
          (setf c (not c))))
      (setf j i))
    c))

(defun line-inside-polygon-p (line-points polygon-points &key do-point-tests)
  "Given a line (two points) determine if the line falls completely inside the
  polygon or not.
  
  If :do-points-tests is t, more accurate line testing is done to determine if
  the line is truly in the polygon via checking points along the line itself.
  This is more accurate (although usually not needed) but much less efficient."
  (declare (type list line-points)
           (type vector polygon-points)
           (type boolean do-point-tests))
  (let ((lx1 (car line-points))
        (ly1 (cadr line-points))
        (lx2 (caddr line-points))
        (ly2 (cadddr line-points)))
    (dotimes (i (length polygon-points))
      (let ((cur-point (aref polygon-points i))
            (next-point (aref polygon-points (if (<= (length polygon-points) (1+ i)) 0 (1+ i)))))
        (unless (and (eq lx1 (car cur-point))
                     (eq ly1 (cadr cur-point))
                     (eq lx2 (car next-point))
                     (eq ly2 (cadr next-point)))
          (when (lines-intersect-p lx1 ly1 lx2 ly2
                                   (car cur-point) (cadr cur-point) (car next-point) (cadr next-point))
            ;(format t "Got intersection in poly: ~a ~a~%" (list (list lx1 ly1) (list lx2 ly2)) (list cur-point next-point))
            (return-from line-inside-polygon-p nil))
          (when do-point-tests
            (let* ((search-res 5)
                   (x-inc (/ (- lx2 lx1) (1+ search-res)))
                   (y-inc (/ (- ly2 ly1) (1+ search-res))))
              (dotimes (i search-res)
                (let ((x (+ lx1 (* (1+ i) x-inc)))
                      (y (+ ly1 (* (1+ i) y-inc))))
                  ;(format t "x,y: ~a,~a~%" x y)
                  (unless (point-in-polygon-p (list x y polygon-points) polygon-points)
                    ;(format t "Point ~a,~a is not inside poly.~%" x y)
                    (return-from line-inside-polygon-p nil))))))))))
  t)

(defun polygon-clockwise-p (polygon-points)
  "Determine if the points of a polygon are in clockwise order."
  (declare (type vector polygon-points))
  (let ((sum 0))
    (loop for i from 0 to (1- (length polygon-points))
          for cur-point = (aref polygon-points i)
          for next-point = (aref polygon-points (mod (1+ i) (length polygon-points))) do
      (incf sum (- (* (car cur-point) (cadr next-point))
                   (* (cadr cur-point) (car next-point)))))
    (< sum 0)))

(defun remove-array-element (array item)
  "Remove an item from an array."
  (let ((c 0)
        (l (length array)))
    (dotimes (i l)
      (let ((cur (aref array i)))
        (if (eq item cur)
            (incf c)
            (when (< 0 c)
              (setf (aref array (- i c)) cur)))))
    (adjust-array array (- l c))))

(defun triangulate (points &key do-point-tests)
  "Given an array of points #((x1 y1) (x2 y2) ...), return a list of triangles
  that constitute the greater object:

    '(((x1 y1) (x2 y2) (x3 y3)) ...)

  The idea is that you can take a polygon, and break it into triangles which
  makes displaying in something like OpenGL extremely trivial.

  The findings can sometime be innacurate, in which case passing :do-point-tests
  as t will perform more accurate testing on the lines within the clipping
  algorithm. It is more accurate (especially for more complex polygons), but 
  much less efficient."
  (assert (vectorp points))
  (let ((points (if (polygon-clockwise-p points) (reverse points) points))
        (triangles nil)
        (last-length -1)
        (last-length-count 0))
    (loop for i from 0 do
      (setf i (mod i (length points)))
      (when (eq (length points) 3)
        (let ((p1 (aref points 0))
              (p2 (aref points 1))
              (p3 (aref points 2)))
          (push (list (list (car p1) (cadr p1))
                      (list (car p2) (cadr p2))
                      (list (car p3) (cadr p3)))
                triangles))
        (return))
      (if (equal (length points) last-length)
          (incf last-length-count)
          (progn (setf last-length-count 0)
                 (setf last-length (length points))))
      (when (< (length points) last-length-count)
        ;(format t "Endless loop with points: ~a~%" points)
        (error 'triangulation-loop :points points))
      (let ((cur-point (aref points i))
            (next-point (aref points (mod (1+ i) (length points))))
            (next-next-point (aref points (mod (+ 2 i) (length points)))))
        ;(format t "~a ~a ~a~%" cur-point next-point next-next-point)
        ;(format t "~a ~a ~a angle: ~a~%" cur-point next-point next-next-point (angle-between-three-points cur-point next-point next-next-point))
        ;; check two things: that the angle of the current ear we're trimming 
        ;; is < 180, and that 
        (when (and (< (angle-between-three-points cur-point next-point next-next-point) 178)
                   (line-inside-polygon-p (list (car cur-point) (cadr cur-point)
                                                (car next-next-point) (cadr next-next-point))
                                          points
                                          :do-point-tests do-point-tests))
          ;(format t "Made it: ~a, ~a~%" cur-point next-next-point)
          (push (list cur-point
                      next-point
                      next-next-point)
                triangles)
          (setf points (remove-array-element points next-point))
          ;(setf points (remove-if (lambda (p) (eq p next-point)) points))
          (decf i 1))))
    triangles))
