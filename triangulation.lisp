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

(defun normalize (v)
  "Normalize a 2D vector"
  (let ((x (car v))
        (y (cadr v)))
    (let ((length (sqrt (+ (expt x 2) (expt y 2)))))
      (list (/ x length) (/ y length)))))

(defun dot-prod (v1 v2)
  "Give the dot product of two 2D vectors."
  (+ (* (car v1) (car v2))
     (* (cadr v1) (cadr v2))))

(defun vec-sub (v1 v2)
  "Subtract two vectors"
  (list (- (car v1) (car v2))
        (- (cadr v1) (cadr v2))))

(defun angle-between-three-points (p1 p2 p3)
  "Given three points, calculate the angle between p1 and p3 with p2 being the
  origin."
  (let* ((x2 (car p2))
         (y2 (cadr p2))
         (x1 (- (car p1) x2))
         (y1 (- (cadr p1) y2))
         (x3 (- (car p3) x2))
         (y3 (- (cadr p3) y2)))
    (let ((v1 (normalize (list x1 y1)))
          (v2 (normalize (list x3 y3))))
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

(defun line-inside-polygon-p (line polygon-points &key do-point-tests)
  "Given a line (two points) determine if the line falls completely inside the
  polygon or not.
  
  If :do-points-tests is t, more accurate line testing is done to determine if
  the line is truly in the polygon via checking points along the line itself.
  This is more accurate (although usually not needed) but much less efficient."
  (let ((lx1 (car (car line)))
        (ly1 (cadr (car line)))
        (lx2 (car (cadr line)))
        (ly2 (cadr (cadr line))))
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

(defun point-in-triangle-p_ (p triangle-points)
  "Test if a point is inside of a triangle."
  (let ((a (aref triangle-points 0))
        (b (aref triangle-points 1))
        (c (aref triangle-points 2)))
    (let ((v0 (normalize (vec-sub c a)))
          (v1 (normalize (vec-sub b a)))
          (v2 (normalize (vec-sub p a))))
      (let ((dot00 (dot-prod v0 v0))
            (dot01 (dot-prod v0 v1))
            (dot02 (dot-prod v0 v2))
            (dot11 (dot-prod v1 v1))
            (dot12 (dot-prod v1 v2)))
        ;(format t "00: ~a~%01: ~a~%02: ~a~%11: ~a~%12: ~a~%" dot00 dot01 dot02 dot11 dot12)
        (let* ((div (- (* dot00 dot11) (* dot01 dot01)))
               (denom (/ 1 (if (zerop div) most-positive-single-float div)))
               (u (* denom (- (* dot11 dot02) (* dot01 dot12))))
               (v (* denom (- (* dot00 dot12) (* dot01 dot02)))))
          ;(format t "u: ~a, v: ~a, +: ~a~%" u v (+ u v))
          (values (and (>= u 0)
                       (>= v 0)
                       (< (+ u v) 1))
                  (list u v)))))))

(defun point-in-triangle-p (p triangle-points)
  "Test if a point is inside of a triangle."
  (let ((a (aref triangle-points 0))
        (b (aref triangle-points 1))
        (c (aref triangle-points 2)))
    (flet ((area (a b c)
             (let ((ax (car a)) (ay (cadr a))
                   (bx (car b)) (by (cadr b))
                   (cx (car c)) (cy (cadr c)))
             (/ (- (* (- ax cx) (- by cy))
                   (* (- bx cx) (- ay cy)))
                2))))
      (let ((total (area a b c))
            (pbc (area p b c))
            (pac (area p a c))
            (pab (area p a b)))
        (values (< (+ pbc pac pab) total)
                total pbc pac pab)))))

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
  (let* ((c 0)
         (l (length array)))
    (dotimes (i l)
      (let ((cur (aref array i)))
        (if (eq item cur)
            (incf c)
            (when (< 0 c)
              (setf (aref array (- i c)) cur)))))
    (adjust-array array (- l c))))

(defun any-points-in-triangle-p (points triangle)
  "Does a point-in-triangle-p test for each point in the polygon for the given
  triangle."
  (let* ((intersect nil)
         (cur-point (aref triangle 0))
         (next-point (aref triangle 1))
         (next-next-point (aref triangle 2))
         (filtered-points (remove-if (lambda (p) (or (eq p cur-point)
                                                     (eq p next-point)
                                                     (eq p next-next-point))) points)))
    (loop for p across filtered-points do
          (unless (or (eq p cur-point)
                      (eq p next-point)
                      (eq p next-next-point)))
          (setf intersect (point-in-triangle-p p triangle))
          (when intersect (return)))
    ;(format t "points inside: ~a ~a~%~a~%" triangle intersect filtered-points)
    intersect))

(defparameter *last-triangulation-run-log* nil
  "This holds a complete version of the polygon for each step of the 
  triangulation, which can be incredibly useful for debugging or visualizing
  the finished polygon without writing it to OpenGL or something (which is still
  tricky to visualize since you need to space your triangles apart from each
  other).")

(defun triangulate (points &key do-point-tests debug)
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
  (when debug (setf *last-triangulation-run-log* nil))

  ;; copy the points (don't destroy original array) and setup for the
  ;; triangulation loop
  (let ((points (map 'vector (lambda (x) x) (if (polygon-clockwise-p points) (reverse points) points)))
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
      (when (> last-length-count (length points))
        (push points *last-triangulation-run-log*)
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
        (when (and (< (angle-between-three-points cur-point next-point next-next-point) 179)
                   (not (any-points-in-triangle-p  points (vector cur-point next-point next-next-point)))
                   (line-inside-polygon-p (list cur-point next-next-point) points :do-point-tests do-point-tests))
          ;(format t "Made it, clipping: ~a~%" next-point)
          ;; write the current point data into the log
          (when debug
            (push points *last-triangulation-run-log*))

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

(defun svg-from-polygons (polygons &key displace random-colors flip-y cols)
  "Given a list of polygons (a polygon is a vector of list point pairs), create
  an SVG string that displays all of the polygons in different colors for easy
  visualization. Great for debugging with *last-triangulation-run-log*."
  (let ((colors '("red" "green" "blue" "orange" "yellow" "red" "pink" "navy"))
        (c 0)
        (displace-cols (if cols cols (round (sqrt (length polygons))))))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" standalone=\"no\"?>
              <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
              <svg with=\"744\" height=\"1052\">~%")
      (dolist (p polygons)
        (let ((color (if random-colors (nth (mod c (length colors)) colors) "#677821"))
              (displace-x (if displace (* displace (mod c displace-cols)) 0))
              (displace-y (if displace (* displace (floor (/ c displace-cols))))))
          (when displace
            (let* ((pt (aref p 0))
                   (x (car pt))
                   (y (if flip-y (- (cadr pt)) (cadr pt)))
                   (dx (+ (- displace-x 10) x))
                   (dy (+ (- displace-y 10) y)))
              (format s "<text x=\"~a\" y=\"~a\" font-size=\"12\">~a~a (~a, ~a)</text>~%" dx dy "P" c x y)))
          (incf c)
          (format s "<polygon fill=\"~a\" style=\"opacity: ~a;\" points=\"" color (if displace 1 .3))
          (loop for i from 0 for (x y) across p do
            (when (zerop (mod i 3)) (format s "~%      "))
            (format s "~a,~a " (+ x displace-x) (+ (if flip-y (- y) y) displace-y)))
          (format s "\" />~%")
          (loop for i from 0 for (x y) across p do
            (format s "<rect x=\"~a\" y=\"~a\" width=\".05\" height=\".05\" fill=\"#000000\" />~%" (+ x displace-x) (+ (if flip-y (- y) y) displace-y)))))
      (format s "</svg>"))))

(defun write-last-triangulation-to-file (path &key displace random-colors flip-y cols)
  "Wonderful debugging function that helps pinpoint where triangulation failed.
  Outputs a (quite beautiful) visual breakdown of the triangulation from start
  to finish (or start to error) into an SVG file (open with inkscape or AI or
  whatever). It's mainly for me to fix triangulation errors, but it can also
  output quite a pretty picture of what's going on under the hood."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s (svg-from-polygons (reverse *last-triangulation-run-log*) :displace displace :random-colors random-colors :flip-y flip-y :cols cols))))

