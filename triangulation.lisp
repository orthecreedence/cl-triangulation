(defpackage :cl-triangulation
  (:use :cl)
  (:export :triangulate
           :polygon-clockwise-p
           :triangulation-loop-points
           :write-last-triangulation-to-file))
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

(defun point-in-triangle-p (p triangle-points)
  "Test if a point is inside of a triangle."
  (let ((a (aref triangle-points 0))
        (b (aref triangle-points 1))
        (c (aref triangle-points 2)))
    (flet ((area (a b c)
             (let ((ax (car a)) (ay (cadr a))
                   (bx (car b)) (by (cadr b))
                   (cx (car c)) (cy (cadr c)))
             (/ (abs (- (+ (* ax by) (* bx cy) (* cx ay))
                        (* ax cy) (* cx by) (* bx ay))) 2))))
      (< (abs (- (+ (area p a b)
                    (area p b c)
                    (area p a c))
                 (area a b c)))
         0.0000000001))))

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

(defparameter *last-triangulation-run-log* nil
  "This holds a complete version of the polygon for each step of the 
  triangulation, which can be incredibly useful for debugging or visualizing
  the finished polygon without writing it to OpenGL or something (which is still
  tricky to visualize since you need to space your triangles apart from each
  other).")

(defun triangulate (points)
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

  ;; clear the log
  (setf *last-triangulation-run-log* nil)

  (let ((points (if (polygon-clockwise-p points) (reverse points) points))
        (triangles nil)
        (last-length -1)
        (last-length-count 0))
    (loop for i from 0 do
      (setf i (mod i (length points)))

      ;; check if we're on our last triangle
      (when (eq (length points) 3)
        (push (list (aref points 0)
                    (aref points 1)
                    (aref points 2))
              triangles)
        (return))

      ;; make sure we aren't in an infinite loop
      (if (equal (length points) last-length)
          (incf last-length-count)
          (progn (setf last-length-count 0)
                 (setf last-length (length points))))
      (when (< (length points) last-length-count)
        ;(format t "Endless loop with points: ~a~%" points)
        (error 'triangulation-loop :points points))

      ;; do our ear clipping here
      (let ((cur-point (aref points i))
            (next-point (aref points (mod (1+ i) (length points))))
            (next-next-point (aref points (mod (+ 2 i) (length points)))))
        ;(format t "~a ~a ~a~%" cur-point next-point next-next-point)
        ;(format t "~a ~a ~a angle: ~a~%" cur-point next-point next-next-point (angle-between-three-points cur-point next-point next-next-point))
        ;; check two things: that the angle of the current ear we're trimming 
        ;; is < 180, and that none of the points in the polygon are inside the
        ;; triangle we're about to clip
        (when (and (< (angle-between-three-points cur-point next-point next-next-point) 178)
                   (let ((intersect nil)
                         (tri (vector cur-point next-point next-next-point)))
                     (loop for p across points do
                       (setf intersect (point-in-triangle-p p tri))
                       (when intersect (return)))
                     (not intersect)))
          ;; write the current point data into the log
          (push points *last-triangulation-run-log*)

          ;(format t "Made it: ~a, ~a~%" cur-point next-next-point)
          (push (list cur-point
                      next-point
                      next-next-point)
                triangles)

          ;; remove the middle point form the point list (ie clip the ear)
          (setf points (remove-array-element points next-point))

          ;(setf points (remove-if (lambda (p) (eq p next-point)) points))
          (decf i 1))))
    triangles))

(defun svg-from-polygons (polygons)
  "Given a list of polygons (a polygon is a vector of list point pairs), create
  an SVG string that displays all of the polygons in different colors for easy
  visualization. Great for debugging with *last-triangulation-run-log*."
  (let ((colors '("red" "green" "blue" "orange" "yellow" "red" "pink" "navy"))
        (c 0))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" standalone=\"no\"?>
              <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
              <svg with=\"744\" height=\"1052\">~%")
      (dolist (p polygons)
        (let ((color (nth (mod c (length colors)) colors)))
          (incf c)
          (format s "<polygon fill=\"~a\" style=\"opacity: .3;\" points=\"" color))
        (loop for i from 0 for (x y) across p do
          (when (zerop (mod i 3)) (format s "~%      "))
          (format s "~a,~a " x y))
        (format s "\" />~%"))
      (format s "</svg>"))))

(defun write-last-triangulation-to-file (path)
  "Wonderful debugging function that helps pinpoint where triangulation failed.
  Outputs a (quite beautiful) visual breakdown of the triangulation from start
  to finish (or start to error) into an SVG file (open with inkscape or AI or
  whatever). It's mainly for me to fix triangulation errors, but it can also
  output quite a pretty picture of what's going on under the hood."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s (svg-from-polygons (reverse *last-triangulation-run-log*)))))

